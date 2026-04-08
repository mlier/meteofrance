module Weather.Fr.MeteoFrance.DPClim
  ( -- * Initialisation
    initDb
  , loadHistory
    -- * Mise à jour
  , updateRecent
    -- * Requêtes
  , getStations
  , getHourlyObs
  , getDailyObs
  , geoSearch
    -- * API DPClim
  , placeOrder
  , downloadFile
  , parseHourlyCSV
  , parseDailyCSV
    -- * Types
  , Station(..)
  , StationId
  , StationStatus(..)
  , HourlyObs(..)
  , DailyObs(..)
  , ApiConfig(..)
  , ApiError(..)
  , Frequency(..)
  , CommandeId(..)
  ) where

import Weather.Fr.MeteoFrance.DPClim.Schema  (initDb)
import Weather.Fr.MeteoFrance.DPClim.Types
import Weather.Fr.MeteoFrance.DPClim.Init    (loadHistory)
import Weather.Fr.MeteoFrance.DPClim.Update  (updateRecent)
import Weather.Fr.MeteoFrance.DPClim.Query
  (getStations, getHourlyObs, getDailyObs, geoSearch)
import Weather.Fr.MeteoFrance.DPClim.Api
  (placeOrder, downloadFile, parseHourlyCSV, parseDailyCSV)
