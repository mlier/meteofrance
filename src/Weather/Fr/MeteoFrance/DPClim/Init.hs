module Weather.Fr.MeteoFrance.DPClim.Init
  ( loadHistory
  ) where

import qualified Codec.Compression.GZip   as GZip
import           Control.Exception         (SomeException, try)
import           Control.Monad             (forM_, when)
import           Data.Aeson                (FromJSON(..), withObject, (.:), eitherDecode)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Csv.Streaming        as CSVS
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time                 (getCurrentTime, utctDay)
import           Data.Time.Calendar        (toGregorian)
import           Database.SQLite.Simple
import           Network.HTTP.Client       (Manager, newManager)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Simple       hiding (Query)
import           Weather.Fr.MeteoFrance.DPClim.Csv
import           Weather.Fr.MeteoFrance.DPClim.Types

-- ---------------------------------------------------------------------------
-- Identifiants datasets data.gouv.fr
-- À vérifier sur https://www.data.gouv.fr/fr/datasets/
-- ---------------------------------------------------------------------------

hourlyDatasetId :: Text
hourlyDatasetId = "6569b4473bedf2e7abad3b72"

-- Dataset quotidien — à confirmer
dailyDatasetId :: Text
dailyDatasetId = "6569b51f3bedf2e7abad3b73"

batchSize :: Int
batchSize = 10000

-- ---------------------------------------------------------------------------
-- Types JSON — API data.gouv.fr /api/1/datasets/{id}/resources/
-- ---------------------------------------------------------------------------

data DatagouvResource = DatagouvResource
  { drUrl   :: !Text
  , drTitle :: !Text
  } deriving (Show)

instance FromJSON DatagouvResource where
  parseJSON = withObject "DatagouvResource" $ \o -> DatagouvResource
    <$> o .: "url"
    <*> o .: "title"

newtype DatagouvPage = DatagouvPage
  { dpData :: [DatagouvResource]
  } deriving (Show)

instance FromJSON DatagouvPage where
  parseJSON = withObject "DatagouvPage" $ \o ->
    DatagouvPage <$> o .: "data"

-- ---------------------------------------------------------------------------
-- SQL
-- ---------------------------------------------------------------------------

sqlInsertStation :: Query
sqlInsertStation =
  "INSERT OR IGNORE INTO stations \
  \(station_id, name, latitude, longitude, altitude, opening_date, closing_date) \
  \VALUES (?,?,?,?,?,?,?)"

sqlInsertHourly :: Query
sqlInsertHourly =
  "INSERT OR IGNORE INTO hourly_observations \
  \(station_id, observed_at, temperature, precipitation_1h, wind_speed, humidity, source) \
  \VALUES (?,?,?,?,?,?,?)"

sqlInsertDaily :: Query
sqlInsertDaily =
  "INSERT OR IGNORE INTO daily_observations \
  \(station_id, observed_at, t_min, t_max, precipitation_24h, sunshine_duration, source) \
  \VALUES (?,?,?,?,?,?,?)"

flushHourly :: Connection -> [(Station, HourlyObs)] -> IO ()
flushHourly conn pairs = withTransaction conn $
  forM_ pairs $ \(s, o) -> do
    execute conn sqlInsertStation (toRow s)
    execute conn sqlInsertHourly  (toRow o)

flushDaily :: Connection -> [(Station, DailyObs)] -> IO ()
flushDaily conn pairs = withTransaction conn $
  forM_ pairs $ \(s, o) -> do
    execute conn sqlInsertStation (toRow s)
    execute conn sqlInsertDaily   (toRow o)

-- ---------------------------------------------------------------------------
-- Parsing CSV en streaming (délègue à Csv.hs)
-- ---------------------------------------------------------------------------

processHourly :: Connection -> CSVS.Records HourlyRow -> IO Int
processHourly conn = go [] 0
  where
    go batch n (CSVS.Cons (Left _)    rest) = go batch n rest
    go batch n (CSVS.Cons (Right row) rest) =
      case hourlyRowToPair row of
        Nothing   -> go batch n rest
        Just pair ->
          let batch' = pair : batch
          in if length batch' >= batchSize
               then flushHourly conn (reverse batch') >> go [] (n + length batch') rest
               else go batch' n rest
    go batch n (CSVS.Nil _ _) = do
      when (not (null batch)) $ flushHourly conn (reverse batch)
      return (n + length batch)

processDaily :: Connection -> CSVS.Records DailyRow -> IO Int
processDaily conn = go [] 0
  where
    go batch n (CSVS.Cons (Left _)    rest) = go batch n rest
    go batch n (CSVS.Cons (Right row) rest) =
      case dailyRowToPair row of
        Nothing   -> go batch n rest
        Just pair ->
          let batch' = pair : batch
          in if length batch' >= batchSize
               then flushDaily conn (reverse batch') >> go [] (n + length batch') rest
               else go batch' n rest
    go batch n (CSVS.Nil _ _) = do
      when (not (null batch)) $ flushDaily conn (reverse batch)
      return (n + length batch)

-- ---------------------------------------------------------------------------
-- HTTP
-- ---------------------------------------------------------------------------

downloadGz :: Manager -> Text -> IO (Either String BL.ByteString)
downloadGz mgr url = do
  r <- try $ do
    req  <- parseRequest (T.unpack url)
    resp <- httpLBS (setRequestManager mgr req)
    return $ GZip.decompress (getResponseBody resp)
  return $ case r of
    Left  e  -> Left (show (e :: SomeException))
    Right bs -> Right bs

-- ---------------------------------------------------------------------------
-- API data.gouv.fr : liste des ressources
-- ---------------------------------------------------------------------------

fetchResources :: Manager -> Text -> IO [(Text, Text)]
fetchResources mgr datasetId = go (1 :: Int) []
  where
    go page acc = do
      let url = "https://www.data.gouv.fr/api/1/datasets/"
                ++ T.unpack datasetId
                -- ++ "/resources/?page=" ++ show page ++ "&page_size=100"
      putStrLn $ "[Init] Url : " ++ url
      r <- try $ do
        req  <- parseRequest url
        resp <- httpLBS (setRequestManager mgr req)
        return (getResponseBody resp)
      case r of
        Left e -> do
          putStrLn $ "[Init] Erreur HTTP: " ++ show (e :: SomeException)
          return acc
        Right body ->
          case eitherDecode body of
            Left err -> do
              putStrLn $ "[Init] Erreur JSON: " ++ err
              return acc
            Right (DatagouvPage items) ->
              if null items
                then return acc
                else go (page + 1)
                         (acc ++ map (\res -> (drTitle res, drUrl res)) items)

filterCsvGz :: [Integer] -> [(Text, Text)] -> [(Text, Text)]
filterCsvGz years = filter matches
  where
    matches (title, url) =
      (".csv.gz" `T.isSuffixOf` url || ".csv.gz" `T.isSuffixOf` title)
      && any (\y -> T.pack (show y) `T.isInfixOf` title) years

-- ---------------------------------------------------------------------------
-- Point d'entrée public
-- ---------------------------------------------------------------------------

-- | Charge 6 ans de données historiques depuis data.gouv.fr.
loadHistory :: Connection -> IO ()
loadHistory conn = do
  putStrLn "[Init] Chargement historique depuis data.gouv.fr..."
  mgr <- newManager tlsManagerSettings
  today <- utctDay <$> getCurrentTime
  let (currentYear, _, _) = toGregorian today
  let targetYears          = [currentYear - 5 .. currentYear]

  -- Horaire
  putStrLn "[Init] Ressources horaires..."
  hRes   <- fetchResources mgr hourlyDatasetId
  let hFiles = filterCsvGz targetYears hRes
  putStrLn $ "[Init] " ++ show (length hFiles) ++ " fichier(s) horaires"
  forM_ hFiles $ \(title, url) -> do
    putStr $ "[Init]   " ++ T.unpack title ++ " ... "
    dl <- downloadGz mgr url
    case dl of
      Left  err -> putStrLn $ "ERREUR: " ++ err
      Right bs  -> case CSVS.decodeByNameWith csvOpts bs of
        Left  err         -> putStrLn $ "CSV ERREUR: " ++ err
        Right (_, records) -> do
          n <- processHourly conn records
          putStrLn $ show n ++ " lignes"

  -- Quotidien
  putStrLn "[Init] Ressources quotidiennes..."
  dRes   <- fetchResources mgr dailyDatasetId
  let dFiles = filterCsvGz targetYears dRes
  putStrLn $ "[Init] " ++ show (length dFiles) ++ " fichier(s) quotidiens"
  forM_ dFiles $ \(title, url) -> do
    putStr $ "[Init]   " ++ T.unpack title ++ " ... "
    dl <- downloadGz mgr url
    case dl of
      Left  err -> putStrLn $ "ERREUR: " ++ err
      Right bs  -> case CSVS.decodeByNameWith csvOpts bs of
        Left  err          -> putStrLn $ "CSV ERREUR: " ++ err
        Right (_, records) -> do
          n <- processDaily conn records
          putStrLn $ show n ++ " lignes"

  putStrLn "[Init] Terminé."
