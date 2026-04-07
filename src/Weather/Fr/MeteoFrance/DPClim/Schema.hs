module Weather.Fr.MeteoFrance.DPClim.Schema
  ( initDb
  ) where

import Database.SQLite.Simple       (Connection, execute_, open)
import Database.SQLite.Simple.Types (Query(..))

-- | Ouvre (ou crée) la base SQLite et s'assure que les 3 tables existent.
initDb :: FilePath -> IO Connection
initDb path = do
  conn <- open path
  applyPragmas conn
  createTables conn
  return conn

applyPragmas :: Connection -> IO ()
applyPragmas conn = do
  execute_ conn "PRAGMA journal_mode = WAL"
  execute_ conn "PRAGMA synchronous  = OFF"
  execute_ conn "PRAGMA foreign_keys = ON"

createTables :: Connection -> IO ()
createTables conn = do
  execute_ conn sqlStations
  execute_ conn sqlHourly
  execute_ conn sqlDaily

sqlStations :: Query
sqlStations = "\
  \CREATE TABLE IF NOT EXISTS stations (\
  \  station_id   TEXT    NOT NULL PRIMARY KEY,\
  \  name         TEXT    NOT NULL,\
  \  latitude     REAL    NOT NULL,\
  \  longitude    REAL    NOT NULL,\
  \  altitude     INTEGER NOT NULL,\
  \  opening_date TEXT    NOT NULL,\
  \  closing_date TEXT\
  \)"

sqlHourly :: Query
sqlHourly = "\
  \CREATE TABLE IF NOT EXISTS hourly_observations (\
  \  station_id      TEXT    NOT NULL REFERENCES stations(station_id),\
  \  observed_at     TEXT    NOT NULL,\
  \  temperature     REAL,\
  \  precipitation_1h REAL,\
  \  wind_speed      REAL,\
  \  humidity        INTEGER,\
  \  source          TEXT    NOT NULL DEFAULT 'CSV',\
  \  PRIMARY KEY (station_id, observed_at)\
  \)"

sqlDaily :: Query
sqlDaily = "\
  \CREATE TABLE IF NOT EXISTS daily_observations (\
  \  station_id        TEXT    NOT NULL REFERENCES stations(station_id),\
  \  observed_at       TEXT    NOT NULL,\
  \  t_min             REAL,\
  \  t_max             REAL,\
  \  precipitation_24h REAL,\
  \  sunshine_duration INTEGER,\
  \  source            TEXT    NOT NULL DEFAULT 'CSV',\
  \  PRIMARY KEY (station_id, observed_at)\
  \)"
