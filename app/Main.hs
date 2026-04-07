module Main where

import Control.Concurrent        (threadDelay)
import Control.Monad             (forever)
import Data.Text                 (pack)
import System.Environment        (lookupEnv)
import System.IO                 (hSetBuffering, stdout, BufferMode(..))
import System.Cron.Schedule      (execSchedule, addJob)
import Weather.Fr.MeteoFrance.DPClim

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  -- Token Bearer depuis variable d'environnement METEO_TOKEN
  mToken <- lookupEnv "METEO_TOKEN"
  token  <- case mToken of
    Nothing -> do
      putStrLn "[Main] AVERTISSEMENT: METEO_TOKEN non défini. updateRecent sera inopérant."
      return ""
    Just t -> do
      putStrLn $ "[Main] Token chargé (" ++ show (length t) ++ " caractères)"
      return t

  let cfg = ApiConfig
        { apiToken        = pack token
        , apiBaseUrl      = "https://public-api.meteofrance.fr/public/DPClim/v1"
        , apiInitialWait  = 5    -- secondes avant premier poll
        , apiPollInterval = 3    -- secondes entre polls
        , apiMaxRetries   = 20   -- retries max
        }

  -- Initialisation base SQLite
  conn <- initDb "meteo.db"
  putStrLn "[Main] Base SQLite initialisée (meteo.db)"

  -- Chargement historique si la base est vide
  stations <- getStations conn Nothing
  if null stations
    then do
      putStrLn "[Main] Base vide — chargement historique data.gouv.fr..."
      loadHistory conn
    else
      putStrLn $ "[Main] " ++ show (length stations)
                ++ " station(s) en base — chargement historique ignoré"

  -- Cron quotidien à 05h00 UTC : "0 5 * * *"
  putStrLn "[Main] Démarrage du cron quotidien (05h00 UTC)..."
  _ <- execSchedule $ addJob (updateRecent cfg conn) "0 5 * * *"

  putStrLn "[Main] Système opérationnel. Ctrl+C pour arrêter."
  forever $ threadDelay (24 * 3600 * 1000000)
