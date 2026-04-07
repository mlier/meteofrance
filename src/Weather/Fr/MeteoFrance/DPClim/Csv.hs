-- | Types CSV intermédiaires et fonctions de conversion partagées
-- entre Init.hs (chargement historique) et Api.hs (mise à jour DPClim).
module Weather.Fr.MeteoFrance.DPClim.Csv
  ( -- * Types CSV Météo-France
    HourlyRow(..)
  , DailyRow(..)
    -- * Helpers
  , parseMF
  , bsToText
  , csvOpts
    -- * Conversions vers types métier
  , hourlyRowToStation
  , hourlyRowToPair      -- (Station, HourlyObs) — pour Init
  , hourlyRowToObs       -- HourlyObs seul       — pour Api
  , dailyRowToStation
  , dailyRowToPair       -- (Station, DailyObs)  — pour Init
  , dailyRowToObs        -- DailyObs seul        — pour Api
  ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char             (ord)
import qualified Data.Csv              as CSV
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as TE
import           Data.Time             (UTCTime(..), utctDay)
import           Data.Time.Format      (parseTimeM, defaultTimeLocale)
import           Weather.Fr.MeteoFrance.DPClim.Types

-- ---------------------------------------------------------------------------
-- Options CSV Météo-France (séparateur `;`)
-- ---------------------------------------------------------------------------

csvOpts :: CSV.DecodeOptions
csvOpts = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord ';') }

-- ---------------------------------------------------------------------------
-- Types CSV intermédiaires
-- Colonnes horaires  : NUM_POSTE;NOM_USUEL;LAT;LON;ALTI;AAAAMMJJHH;T;RR1;FF;U
-- Colonnes quotidien : NUM_POSTE;NOM_USUEL;LAT;LON;ALTI;AAAAMMJJ;TN;TX;RR;INSOLH
-- Valeurs manquantes : champ vide ou "mq"
-- ---------------------------------------------------------------------------

-- | Champ optionnel Météo-France : vide ou "mq" → Nothing
parseMF :: CSV.FromField a => BS.ByteString -> CSV.Parser (Maybe a)
parseMF bs
  | BS.null bs || bs == "mq" = pure Nothing
  | otherwise                 = Just <$> CSV.parseField bs

data HourlyRow = HourlyRow
  { hrNumPoste :: !BS.ByteString
  , hrNomUsuel :: !BS.ByteString
  , hrLat      :: !Double
  , hrLon      :: !Double
  , hrAlti     :: !Double
  , hrDate     :: !BS.ByteString  -- AAAAMMJJHH ex: "2022061514"
  , hrT        :: !(Maybe Double) -- température (°C)
  , hrRr1      :: !(Maybe Double) -- précipitations 1h (mm)
  , hrFf       :: !(Maybe Double) -- vitesse vent (m/s)
  , hrU        :: !(Maybe Double) -- humidité (%)
  }

instance CSV.FromNamedRecord HourlyRow where
  parseNamedRecord r = HourlyRow
    <$>  r CSV..: "NUM_POSTE"
    <*>  r CSV..: "NOM_USUEL"
    <*>  r CSV..: "LAT"
    <*>  r CSV..: "LON"
    <*>  r CSV..: "ALTI"
    <*>  r CSV..: "AAAAMMJJHH"
    <*> (r CSV..: "T"   >>= parseMF)
    <*> (r CSV..: "RR1" >>= parseMF)
    <*> (r CSV..: "FF"  >>= parseMF)
    <*> (r CSV..: "U"   >>= parseMF)

data DailyRow = DailyRow
  { drNumPoste :: !BS.ByteString
  , drNomUsuel :: !BS.ByteString
  , drLat      :: !Double
  , drLon      :: !Double
  , drAlti     :: !Double
  , drDate     :: !BS.ByteString  -- AAAAMMJJ ex: "20220615"
  , drTn       :: !(Maybe Double) -- température min (°C)
  , drTx       :: !(Maybe Double) -- température max (°C)
  , drRr       :: !(Maybe Double) -- précipitations 24h (mm)
  , drInsolH   :: !(Maybe Double) -- insolation (heures → converti en minutes)
  }

instance CSV.FromNamedRecord DailyRow where
  parseNamedRecord r = DailyRow
    <$>  r CSV..: "NUM_POSTE"
    <*>  r CSV..: "NOM_USUEL"
    <*>  r CSV..: "LAT"
    <*>  r CSV..: "LON"
    <*>  r CSV..: "ALTI"
    <*>  r CSV..: "AAAAMMJJ"
    <*> (r CSV..: "TN"     >>= parseMF)
    <*> (r CSV..: "TX"     >>= parseMF)
    <*> (r CSV..: "RR"     >>= parseMF)
    <*> (r CSV..: "INSOLH" >>= parseMF)

-- ---------------------------------------------------------------------------
-- Utilitaires
-- ---------------------------------------------------------------------------

bsToText :: BS.ByteString -> Text
bsToText bs = case TE.decodeUtf8' bs of
  Right t -> t
  Left  _ -> TE.decodeLatin1 bs

-- ---------------------------------------------------------------------------
-- Conversions
-- ---------------------------------------------------------------------------

hourlyRowToStation :: HourlyRow -> UTCTime -> Station
hourlyRowToStation r ts = Station
  { stationId   = bsToText (hrNumPoste r)
  , stationName = bsToText (hrNomUsuel r)
  , latitude    = hrLat r
  , longitude   = hrLon r
  , altitude    = round (hrAlti r)
  , openingDate = utctDay ts
  , closingDate = Nothing
  }

hourlyRowToObs :: HourlyRow -> Maybe HourlyObs
hourlyRowToObs r = do
  ts <- parseTimeM True defaultTimeLocale "%Y%m%d%H" (BSC.unpack (hrDate r))
  return HourlyObs
    { hoStationId     = bsToText (hrNumPoste r)
    , hoObservedAt    = ts
    , hoTemperature   = hrT r
    , hoPrecipitation = hrRr1 r
    , hoWindSpeed     = hrFf r
    , hoHumidity      = fmap round (hrU r)
    , hoSource        = "API"
    }

-- | Pour Init : produit (Station, HourlyObs) ensemble
hourlyRowToPair :: HourlyRow -> Maybe (Station, HourlyObs)
hourlyRowToPair r = do
  ts  <- parseTimeM True defaultTimeLocale "%Y%m%d%H" (BSC.unpack (hrDate r))
  let obs = HourlyObs
        { hoStationId     = bsToText (hrNumPoste r)
        , hoObservedAt    = ts
        , hoTemperature   = hrT r
        , hoPrecipitation = hrRr1 r
        , hoWindSpeed     = hrFf r
        , hoHumidity      = fmap round (hrU r)
        , hoSource        = "CSV"
        }
  return (hourlyRowToStation r ts, obs)

dailyRowToStation :: DailyRow -> Maybe Station
dailyRowToStation r = do
  d <- parseTimeM True defaultTimeLocale "%Y%m%d" (BSC.unpack (drDate r))
  return Station
    { stationId   = bsToText (drNumPoste r)
    , stationName = bsToText (drNomUsuel r)
    , latitude    = drLat r
    , longitude   = drLon r
    , altitude    = round (drAlti r)
    , openingDate = d
    , closingDate = Nothing
    }

dailyRowToObs :: DailyRow -> Maybe DailyObs
dailyRowToObs r = do
  d <- parseTimeM True defaultTimeLocale "%Y%m%d" (BSC.unpack (drDate r))
  return DailyObs
    { doStationId        = bsToText (drNumPoste r)
    , doObservedAt       = d
    , doTMin             = drTn r
    , doTMax             = drTx r
    , doPrecipitation24h = drRr r
    , doSunshineDuration = fmap (\h -> round (h * 60.0)) (drInsolH r)
    , doSource           = "API"
    }

-- | Pour Init : produit (Station, DailyObs) ensemble
dailyRowToPair :: DailyRow -> Maybe (Station, DailyObs)
dailyRowToPair r = do
  obs     <- dailyRowToObs r
  station <- dailyRowToStation r
  return (station, obs { doSource = "CSV" })
