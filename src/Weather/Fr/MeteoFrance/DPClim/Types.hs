module Weather.Fr.MeteoFrance.DPClim.Types
  ( -- * Station
    Station(..)
  , StationId
  , StationStatus(..)
    -- * Observations
  , HourlyObs(..)
  , DailyObs(..)
    -- * API
  , CommandeId(..)
  , ApiConfig(..)
  , Frequency(..)
  , ApiError(..)
  , StationInfo(..)
  , Parametre(..)
  ) where

import Data.Aeson   (FromJSON(..), withObject, (.:), (.:?), (.!=))
import Data.Text    (Text)
import Data.Time    (Day, UTCTime)
import Database.SQLite.Simple (FromRow(..), ToRow(..), field)

-- ---------------------------------------------------------------------------
-- Station
-- ---------------------------------------------------------------------------

type StationId = Text

data StationStatus = Active | Closed | AllStations
  deriving (Show, Eq)

data Station = Station
  { stationId   :: StationId
  , stationName :: Text
  , latitude    :: Double
  , longitude   :: Double
  , altitude    :: Int
  , openingDate :: Day
  , closingDate :: Maybe Day
  } deriving (Show, Eq)

instance FromRow Station where
  fromRow = Station
    <$> field   -- station_id   TEXT
    <*> field   -- name         TEXT
    <*> field   -- latitude     REAL
    <*> field   -- longitude    REAL
    <*> field   -- altitude     INTEGER
    <*> field   -- opening_date TEXT (YYYY-MM-DD, parsé par sqlite-simple)
    <*> field   -- closing_date TEXT NULLABLE

instance ToRow Station where
  toRow (Station sid nm lat lon alt opd cld) =
    toRow (sid, nm, lat, lon, alt, opd, cld)

-- ---------------------------------------------------------------------------
-- HourlyObs
-- ---------------------------------------------------------------------------

data HourlyObs = HourlyObs
  { hoStationId     :: StationId
  , hoObservedAt    :: UTCTime
  , hoTemperature   :: Maybe Double   -- °C
  , hoPrecipitation :: Maybe Double   -- mm
  , hoWindSpeed     :: Maybe Double   -- m/s
  , hoHumidity      :: Maybe Int      -- %
  , hoSource        :: Text
  } deriving (Show, Eq)

instance FromRow HourlyObs where
  fromRow = HourlyObs
    <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow HourlyObs where
  toRow (HourlyObs sid ts tmp prec wind hum src) =
    toRow (sid, ts, tmp, prec, wind, hum, src)

-- ---------------------------------------------------------------------------
-- DailyObs
-- ---------------------------------------------------------------------------

data DailyObs = DailyObs
  { doStationId        :: StationId
  , doObservedAt       :: Day
  , doTMin             :: Maybe Double   -- °C
  , doTMax             :: Maybe Double   -- °C
  , doPrecipitation24h :: Maybe Double   -- mm
  , doSunshineDuration :: Maybe Int      -- minutes
  , doSource           :: Text
  } deriving (Show, Eq)

instance FromRow DailyObs where
  fromRow = DailyObs
    <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DailyObs where
  toRow (DailyObs sid d tmn tmx prec sun src) =
    toRow (sid, d, tmn, tmx, prec, sun, src)

-- ---------------------------------------------------------------------------
-- API DPClim
-- ---------------------------------------------------------------------------

-- | L'id-cmde est une String dans la réponse JSON réelle :
--   {"elaboreProduitAvecDemandeResponse": {"return": "768920711487"}}
newtype CommandeId = CommandeId { unCommandeId :: Text }
  deriving (Show, Eq)

instance FromJSON CommandeId where
  parseJSON = withObject "CommandeResponse" $ \top -> do
    inner <- top .: "elaboreProduitAvecDemandeResponse"
    CommandeId <$> inner .: "return"

data ApiConfig = ApiConfig
  { apiToken        :: Text   -- Bearer token (env METEO_TOKEN)
  , apiBaseUrl      :: Text   -- "https://public-api.meteofrance.fr/public/DPClim/v1"
  , apiInitialWait  :: Int    -- secondes avant premier poll (défaut 5)
  , apiPollInterval :: Int    -- secondes entre polls (défaut 3)
  , apiMaxRetries   :: Int    -- retries max polling (défaut 20)
  } deriving (Show)

data Frequency = Horaire | Quotidienne
  deriving (Show, Eq)

data ApiError
  = AlreadyDelivered        -- HTTP 410
  | ProductionFailed Text   -- HTTP 500 (période hors activité capteur)
  | GatewayDown             -- 503/303001 → attendre 30s puis retry
  | MaxRetriesReached
  | HttpError Int Text      -- code HTTP inattendu + body
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Métadonnées station (/information-station)
-- Format date MF : "YYYY-MM-DD HH:MM:SS" ; dateFin vide = station active
-- ---------------------------------------------------------------------------

data Parametre = Parametre
  { pNom       :: Text
  , pDateDebut :: Text
  , pDateFin   :: Text
  } deriving (Show, Eq)

instance FromJSON Parametre where
  parseJSON = withObject "Parametre" $ \o -> Parametre
    <$> o .:  "nom"
    <*> o .:  "dateDebut"
    <*> (o .:? "dateFin" .!= "")

data StationInfo = StationInfo
  { siId         :: Int
  , siNom        :: Text
  , siDateDebut  :: Text
  , siDateFin    :: Text
  , siParametres :: [Parametre]
  } deriving (Show, Eq)

instance FromJSON StationInfo where
  parseJSON = withObject "StationInfo" $ \o -> StationInfo
    <$> o .:  "id"
    <*> o .:  "nom"
    <*> o .:  "dateDebut"
    <*> (o .:? "dateFin" .!= "")
    <*> (o .:? "parametres" .!= [])
