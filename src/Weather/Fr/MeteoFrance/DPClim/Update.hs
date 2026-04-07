module Weather.Fr.MeteoFrance.DPClim.Update
  ( updateRecent
  ) where

import           Control.Concurrent.Async   (forConcurrently_)
import           Control.Concurrent.STM
import           Control.Exception          (SomeException, try)
import           Control.Monad              (forM_, when)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Data.Time
import           Database.SQLite.Simple
import           Weather.Fr.MeteoFrance.DPClim.Api
import           Weather.Fr.MeteoFrance.DPClim.Query (getStations)
import           Weather.Fr.MeteoFrance.DPClim.Types

-- ---------------------------------------------------------------------------
-- Paramètres
-- ---------------------------------------------------------------------------

-- | Nombre de stations traitées en parallèle.
-- Limité pour éviter le rate-limiting 429.
concurrencyLimit :: Int
concurrencyLimit = 5

-- | Décalage de sécurité : on n'interroge pas au-delà de J-1.
safetyLagDays :: Integer
safetyLagDays = 1

-- | Découpage max en tranches d'un an (contrainte API DPClim).
maxYearSpan :: Integer
maxYearSpan = 365

-- ---------------------------------------------------------------------------
-- SQL
-- ---------------------------------------------------------------------------

sqlMaxHourly :: Query
sqlMaxHourly =
  "SELECT MAX(observed_at) FROM hourly_observations WHERE station_id = ?"

sqlMaxDaily :: Query
sqlMaxDaily =
  "SELECT MAX(observed_at) FROM daily_observations WHERE station_id = ?"

sqlUpsertHourly :: Query
sqlUpsertHourly =
  "INSERT INTO hourly_observations \
  \(station_id, observed_at, temperature, precipitation_1h, wind_speed, humidity, source) \
  \VALUES (?,?,?,?,?,?,?) \
  \ON CONFLICT(station_id, observed_at) DO UPDATE SET \
  \  temperature      = excluded.temperature, \
  \  precipitation_1h = excluded.precipitation_1h, \
  \  wind_speed       = excluded.wind_speed, \
  \  humidity         = excluded.humidity, \
  \  source           = 'API_CORRECTED'"

sqlUpsertDaily :: Query
sqlUpsertDaily =
  "INSERT INTO daily_observations \
  \(station_id, observed_at, t_min, t_max, precipitation_24h, sunshine_duration, source) \
  \VALUES (?,?,?,?,?,?,?) \
  \ON CONFLICT(station_id, observed_at) DO UPDATE SET \
  \  t_min             = excluded.t_min, \
  \  t_max             = excluded.t_max, \
  \  precipitation_24h = excluded.precipitation_24h, \
  \  sunshine_duration = excluded.sunshine_duration, \
  \  source            = 'API_CORRECTED'"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Découpe [start, end] en tranches de maxYearSpan jours maximum.
-- Contrainte API : période ≤ 1 an glissant.
splitYearSlices :: UTCTime -> UTCTime -> [(UTCTime, UTCTime)]
splitYearSlices start end
  | diffUTCTime end start <= fromIntegral (maxYearSpan * 86400) = [(start, end)]
  | otherwise =
      let sliceEnd = addUTCTime (fromIntegral (maxYearSpan * 86400)) start
      in (start, sliceEnd) : splitYearSlices sliceEnd end

-- | Dernier timestamp horaire connu pour une station (Nothing si table vide).
lastHourlyTs :: Connection -> StationId -> IO (Maybe UTCTime)
lastHourlyTs conn sid = do
  rows <- query conn sqlMaxHourly (Only sid) :: IO [Only (Maybe UTCTime)]
  return $ case rows of
    [Only (Just ts)] -> Just ts
    _                -> Nothing

-- | Dernier jour quotidien connu pour une station.
lastDailyDay :: Connection -> StationId -> IO (Maybe Day)
lastDailyDay conn sid = do
  rows <- query conn sqlMaxDaily (Only sid) :: IO [Only (Maybe Day)]
  return $ case rows of
    [Only (Just d)] -> Just d
    _               -> Nothing

-- | Convertit un Day en UTCTime (début de journée UTC).
dayToUTC :: Day -> UTCTime
dayToUTC d = UTCTime d 0

-- ---------------------------------------------------------------------------
-- Mise à jour d'une station
-- ---------------------------------------------------------------------------

updateStation :: ApiConfig -> Connection -> TVar Int -> UTCTime -> Station -> IO ()
updateStation cfg conn semaphore now station = do
  -- Sémaphore : attendre un slot de concurrence
  atomically $ do
    n <- readTVar semaphore
    if n > 0
      then writeTVar semaphore (n - 1)
      else retry

  result <- try $ updateStationUnsafe cfg conn now station
  case result of
    Left e  -> putStrLn $
      "[Update] " ++ T.unpack (stationId station) ++ " ERREUR: " ++ show (e :: SomeException)
    Right _ -> return ()

  -- Libérer le slot
  atomically $ modifyTVar' semaphore (+1)

updateStationUnsafe :: ApiConfig -> Connection -> UTCTime -> Station -> IO ()
updateStationUnsafe cfg conn now station = do
  let sid    = stationId station
  let endTs  = addUTCTime (negate (fromIntegral (safetyLagDays * 86400))) now

  -- Mise à jour horaire
  mLastH <- lastHourlyTs conn sid
  let startH = fromMaybe (dayToUTC (openingDate station)) mLastH
  when (startH < endTs) $
    updateFreq cfg conn sid startH endTs Horaire

  -- Mise à jour quotidienne
  mLastD <- lastDailyDay conn sid
  let startD = fromMaybe (dayToUTC (openingDate station))
                          (fmap (dayToUTC . addDays 1) mLastD)
  when (startD < endTs) $
    updateFreq cfg conn sid startD endTs Quotidienne

updateFreq :: ApiConfig -> Connection -> StationId -> UTCTime -> UTCTime -> Frequency -> IO ()
updateFreq cfg conn sid start end freq = do
  let slices = splitYearSlices start end
  forM_ slices $ \(s, e) -> do
    result <- try $ placeOrder cfg sid s e freq
    case result of
      Left ex -> putStrLn $
        "[Update] " ++ T.unpack sid ++ " commande " ++ show freq
        ++ " ERREUR: " ++ show (ex :: SomeException)
      Right cmdId -> do
        dl <- downloadFile cfg cmdId
        case dl of
          Left AlreadyDelivered -> return ()  -- déjà traité, skip
          Left (ProductionFailed msg) ->
            putStrLn $ "[Update] " ++ T.unpack sid
              ++ " production en échec: " ++ T.unpack msg
          Left MaxRetriesReached ->
            putStrLn $ "[Update] " ++ T.unpack sid ++ " timeout polling"
          Left GatewayDown ->
            putStrLn $ "[Update] " ++ T.unpack sid ++ " gateway indisponible"
          Left (HttpError code msg) ->
            putStrLn $ "[Update] " ++ T.unpack sid
              ++ " HTTP " ++ show code ++ ": " ++ T.unpack msg
          Right csvBytes ->
            case freq of
              Horaire ->
                case parseHourlyCSV csvBytes of
                  Left  err -> putStrLn $ "[Update] CSV parse erreur: " ++ err
                  Right obs -> withTransaction conn $
                    forM_ obs $ \o -> execute conn sqlUpsertHourly (toRow o)
              Quotidienne ->
                case parseDailyCSV csvBytes of
                  Left  err -> putStrLn $ "[Update] CSV parse erreur: " ++ err
                  Right obs -> withTransaction conn $
                    forM_ obs $ \o -> execute conn sqlUpsertDaily (toRow o)

-- ---------------------------------------------------------------------------
-- Point d'entrée public
-- ---------------------------------------------------------------------------

-- | Met à jour les observations récentes pour toutes les stations.
-- Pour chaque station : commande horaire + quotidienne, polling, UPSERT.
-- Concurrence limitée à concurrencyLimit stations simultanées.
updateRecent :: ApiConfig -> Connection -> IO ()
updateRecent cfg conn = do
  putStrLn "[Update] Début mise à jour..."
  now      <- getCurrentTime
  stations <- getStations conn Nothing   -- toutes stations, y compris fermées
  semaphore <- newTVarIO concurrencyLimit
  let cfg' = cfg

  putStrLn $ "[Update] " ++ show (length stations) ++ " station(s) à traiter"
  forConcurrently_ stations $ updateStation cfg' conn semaphore now

  putStrLn "[Update] Mise à jour terminée."
