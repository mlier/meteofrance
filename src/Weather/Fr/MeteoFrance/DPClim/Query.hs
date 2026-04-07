module Weather.Fr.MeteoFrance.DPClim.Query
  ( getStations
  , getHourlyObs
  , getDailyObs
  , geoSearch
  ) where

import Data.Time (Day)
import Database.SQLite.Simple (Connection, query, query_)
import Weather.Fr.MeteoFrance.DPClim.Types

-- | Liste toutes les stations, avec filtre optionnel sur le statut.
getStations :: Connection -> Maybe StationStatus -> IO [Station]
getStations conn Nothing =
  query_ conn "SELECT station_id, name, latitude, longitude, altitude, opening_date, closing_date FROM stations"
getStations conn (Just Active) =
  query_ conn "SELECT station_id, name, latitude, longitude, altitude, opening_date, closing_date FROM stations WHERE closing_date IS NULL"
getStations conn (Just Closed) =
  query_ conn "SELECT station_id, name, latitude, longitude, altitude, opening_date, closing_date FROM stations WHERE closing_date IS NOT NULL"
getStations conn (Just AllStations) = getStations conn Nothing

-- | Observations horaires d'une station sur une plage de dates.
getHourlyObs :: Connection -> StationId -> Day -> Day -> IO [HourlyObs]
getHourlyObs conn sid from to =
  query conn
    "SELECT station_id, observed_at, temperature, precipitation_1h, wind_speed, humidity, source \
    \FROM hourly_observations \
    \WHERE station_id = ? AND observed_at >= ? AND observed_at <= ? \
    \ORDER BY observed_at"
    (sid, show from, show to)

-- | Observations quotidiennes d'une station sur une plage de dates.
getDailyObs :: Connection -> StationId -> Day -> Day -> IO [DailyObs]
getDailyObs conn sid from to =
  query conn
    "SELECT station_id, observed_at, t_min, t_max, precipitation_24h, sunshine_duration, source \
    \FROM daily_observations \
    \WHERE station_id = ? AND observed_at >= ? AND observed_at <= ? \
    \ORDER BY observed_at"
    (sid, show from, show to)

-- | Recherche géographique : stations dans un rayon (km) autour d'un point GPS.
-- Utilise la formule de Haversine.
geoSearch :: Connection
          -> Double   -- ^ latitude centre (degrés)
          -> Double   -- ^ longitude centre (degrés)
          -> Double   -- ^ rayon (km)
          -> IO [Station]
geoSearch conn lat lon radiusKm = do
  -- On charge toutes les stations et on filtre en mémoire
  -- (volume ~1500 stations, négligeable)
  all_ <- getStations conn Nothing
  return $ filter (withinRadius lat lon radiusKm) all_

withinRadius :: Double -> Double -> Double -> Station -> Bool
withinRadius lat lon radiusKm s =
  haversine lat lon (latitude s) (longitude s) <= radiusKm

-- | Distance haversine en kilomètres entre deux points GPS.
haversine :: Double -> Double -> Double -> Double -> Double
haversine lat1 lon1 lat2 lon2 =
  let r    = 6371.0          -- rayon moyen de la Terre (km)
      dlat = toRad (lat2 - lat1)
      dlon = toRad (lon2 - lon1)
      a    = sin (dlat / 2) ^ (2 :: Int)
           + cos (toRad lat1) * cos (toRad lat2) * sin (dlon / 2) ^ (2 :: Int)
      c    = 2 * atan2 (sqrt a) (sqrt (1 - a))
  in r * c

toRad :: Double -> Double
toRad d = d * pi / 180
