module Weather.Fr.MeteoFrance.DPClim.Api
  ( placeOrder
  , downloadFile
  , parseHourlyCSV
  , parseDailyCSV
  ) where

import           Control.Concurrent        (threadDelay)
import           Control.Exception         (SomeException, try)
import           Data.Aeson                (eitherDecode)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Csv.Streaming        as CSVS
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Encoding.Error  as TEE
import           Data.Time                 (UTCTime)
import           Data.Time.Format          (formatTime, defaultTimeLocale)
import           Network.HTTP.Simple       hiding (Query)
import           Network.HTTP.Types.Status (statusCode)
import           Weather.Fr.MeteoFrance.DPClim.Csv
import           Weather.Fr.MeteoFrance.DPClim.Types

-- ---------------------------------------------------------------------------
-- Formatage des dates pour les paramètres API (ISO 8601 UTC)
-- ---------------------------------------------------------------------------

fmtHourlyStart :: UTCTime -> String
fmtHourlyStart = formatTime defaultTimeLocale "%Y-%m-%dT%H:00:00Z"

fmtHourlyEnd :: UTCTime -> String
fmtHourlyEnd = formatTime defaultTimeLocale "%Y-%m-%dT%H:59:59Z"

fmtDailyStart :: UTCTime -> String
fmtDailyStart = formatTime defaultTimeLocale "%Y-%m-%dT00:00:00Z"

fmtDailyEnd :: UTCTime -> String
fmtDailyEnd = formatTime defaultTimeLocale "%Y-%m-%dT23:59:59Z"

-- ---------------------------------------------------------------------------
-- Placement de commande
-- GET /commande-station/{horaire|quotidienne}
-- Réponse 202 : {"elaboreProduitAvecDemandeResponse": {"return": "<id-cmde>"}}
-- ---------------------------------------------------------------------------

-- | Place une commande et retourne le CommandeId (String dans la réponse JSON).
placeOrder :: ApiConfig
           -> StationId
           -> UTCTime    -- date début
           -> UTCTime    -- date fin
           -> Frequency
           -> IO CommandeId
placeOrder cfg sid start end freq = do
  let (endpoint, startStr, endStr) = case freq of
        Horaire     -> ("horaire",     fmtHourlyStart start, fmtHourlyEnd end)
        Quotidienne -> ("quotidienne", fmtDailyStart  start, fmtDailyEnd  end)
  let url = T.unpack (apiBaseUrl cfg)
            ++ "/commande-station/" ++ endpoint
            ++ "?id-station="       ++ T.unpack sid
            ++ "&date-deb-periode=" ++ startStr
            ++ "&date-fin-periode=" ++ endStr
  req <- parseRequest url
  let req' = setRequestHeader "apikey" [TE.encodeUtf8 (apiKey cfg)] req
  resp <- httpLBS req'
  let code = statusCode (getResponseStatus resp)
  case code of
    202 -> case eitherDecode (getResponseBody resp) of
             Left  err -> ioError $ userError $
               "[Api] Erreur parsing id-cmde: " ++ err
             Right cid -> return cid
    400 -> ioError $ userError $
             "[Api] 400 Bad Request — station ou dates invalides: " ++ T.unpack sid
    _   -> ioError $ userError $
             "[Api] HTTP " ++ show code ++ " pour station " ++ T.unpack sid

-- ---------------------------------------------------------------------------
-- Téléchargement avec polling
-- GET /commande/fichier?id-cmde=<id>
--   204 → encore en production → retry après apiPollInterval secondes
--   201 → CSV prêt → retourner le body
--   410 → déjà livré
--   500 → production en échec (période sans données capteurs)
--   503 → gateway suspendue 30s (erreur 303001) → attendre 30s, retry
-- ---------------------------------------------------------------------------

waitSec :: Int -> IO ()
waitSec n = threadDelay (n * 1000000)

-- | Polling jusqu'à obtenir le CSV ou une erreur définitive.
downloadFile :: ApiConfig -> CommandeId -> IO (Either ApiError BL.ByteString)
downloadFile cfg (CommandeId cid) = do
  waitSec (apiInitialWait cfg)
  poll (apiMaxRetries cfg)
  where
    url = T.unpack (apiBaseUrl cfg)
          ++ "/commande/fichier?id-cmde=" ++ T.unpack cid

    authHeader = TE.encodeUtf8 (apiKey cfg)

    poll 0 = return (Left MaxRetriesReached)
    poll n = do
      r <- try $ do
        req <- parseRequest url
        httpLBS (setRequestHeader "apikey" [authHeader] req)
      case r of
        Left e -> do
          putStrLn $ "[Api] Erreur réseau: " ++ show (e :: SomeException)
          waitSec 30
          poll (n - 1)
        Right resp ->
          case statusCode (getResponseStatus resp) of
            201 -> return $ Right (getResponseBody resp)
            204 -> waitSec (apiPollInterval cfg) >> poll (n - 1)
            410 -> return $ Left AlreadyDelivered
            500 -> return $ Left $ ProductionFailed (bodyText resp)
            503 -> do
              putStrLn "[Api] Gateway indisponible (30s)..."
              waitSec 30
              poll (n - 1)
            code -> return $ Left $ HttpError code (bodyText resp)

    bodyText resp =
      TE.decodeUtf8With TEE.lenientDecode (BL.toStrict (getResponseBody resp))

-- ---------------------------------------------------------------------------
-- Parse du CSV retourné par l'API (délègue à Csv.hs)
-- source = "API" (distinct de "CSV" pour l'historique)
-- ---------------------------------------------------------------------------

-- | Parse le CSV horaire (body 201) → liste HourlyObs.
parseHourlyCSV :: BL.ByteString -> Either String [HourlyObs]
parseHourlyCSV bs =
  case CSVS.decodeByNameWith csvOpts bs of
    Left  err          -> Left err
    Right (_, records) -> Right (collect records)
  where
    collect (CSVS.Cons (Left  _)   rest) = collect rest
    collect (CSVS.Cons (Right row) rest) =
      case hourlyRowToObs row of
        Nothing  -> collect rest
        Just obs -> obs : collect rest
    collect (CSVS.Nil _ _) = []

-- | Parse le CSV quotidien (body 201) → liste DailyObs.
parseDailyCSV :: BL.ByteString -> Either String [DailyObs]
parseDailyCSV bs =
  case CSVS.decodeByNameWith csvOpts bs of
    Left  err          -> Left err
    Right (_, records) -> Right (collect records)
  where
    collect (CSVS.Cons (Left  _)   rest) = collect rest
    collect (CSVS.Cons (Right row) rest) =
      case dailyRowToObs row of
        Nothing  -> collect rest
        Just obs -> obs : collect rest
    collect (CSVS.Nil _ _) = []
