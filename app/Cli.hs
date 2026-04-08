module Main where

import           Brick hiding (str)
import qualified Brick.AttrMap        as BA
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.List   as BL
import           Control.Monad        (void)
import           Data.List            (intercalate)
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import           Data.Text            (Text)
import           Data.Time
import qualified Data.Vector          as Vec
import           Database.SQLite.Simple (Connection)
import qualified Graphics.Vty         as V
import           Options.Applicative
import           Options.Applicative.Help.Pretty (Doc, vsep, pretty)
import           System.Environment   (lookupEnv)
import           System.IO            (hPutStrLn, hSetBuffering, stderr, stdout, BufferMode(..))

import Weather.Fr.MeteoFrance.DPClim

-- ---------------------------------------------------------------------------
-- TUI (brick)
-- ---------------------------------------------------------------------------

data ResourceName = RList deriving (Eq, Ord, Show)

data AppState = AppState
  { appList   :: BL.List ResourceName Text
  , appTitle  :: Text
  , appHeader :: Text
  }

headerAttr :: AttrName
headerAttr = attrName "header"

drawUi :: AppState -> [Widget ResourceName]
drawUi st =
  [ BB.borderWithLabel (txt (appTitle st)) $
      vBox
        [ withAttr headerAttr (txt (appHeader st))
        , BB.hBorder
        , BL.renderList (\_ row -> txt row) True (appList st)
        ]
  ]

handleEvent :: BrickEvent ResourceName () -> EventM ResourceName AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc []))           = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') []))    = halt
handleEvent (VtyEvent (V.EvKey V.KDown []))           =
  modify $ \s -> s { appList = BL.listMoveDown (appList s) }
handleEvent (VtyEvent (V.EvKey V.KUp []))             =
  modify $ \s -> s { appList = BL.listMoveUp (appList s) }
handleEvent (VtyEvent (V.EvKey V.KPageDown []))       =
  modify $ \s -> s { appList = BL.listMoveBy 10 (appList s) }
handleEvent (VtyEvent (V.EvKey V.KPageUp []))         =
  modify $ \s -> s { appList = BL.listMoveBy (-10) (appList s) }
handleEvent _                                         = return ()

theApp :: App AppState () ResourceName
theApp = App
  { appDraw         = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = const $ BA.attrMap V.defAttr
      [ (headerAttr,          V.withStyle V.defAttr V.bold)
      , (BL.listSelectedAttr, V.withStyle V.defAttr V.reverseVideo)
      ]
  }

runTui :: Text -> Text -> [Text] -> IO ()
runTui title hdr rows =
  void $ defaultMain theApp AppState
    { appList   = BL.list RList (Vec.fromList rows) 1
    , appTitle  = title
    , appHeader = hdr
    }

-- ---------------------------------------------------------------------------
-- Formatage colonnes
-- ---------------------------------------------------------------------------

pad :: Int -> String -> String
pad n s = take n (s ++ repeat ' ')

showM :: (a -> String) -> Maybe a -> String
showM f = maybe "-" f

fmtCols :: [Int] -> [String] -> Text
fmtCols widths cols =
  T.pack $ concatMap (uncurry pad) (zip widths cols)

-- Stations : ID(10) Nom(28) Lat(9) Lon(9) Alt(6) Ouverture(12) Fermeture(12)
stationWidths :: [Int]
stationWidths = [10, 28, 9, 9, 6, 12, 12]

stationHeader :: Text
stationHeader = fmtCols stationWidths
  ["ID", "Nom", "Lat", "Lon", "Alt", "Ouverture", "Fermeture"]

stationRow :: Station -> Text
stationRow s = fmtCols stationWidths
  [ T.unpack (stationId s)
  , T.unpack (stationName s)
  , show (latitude s)
  , show (longitude s)
  , show (altitude s)
  , show (openingDate s)
  , showM show (closingDate s)
  ]

-- Horaire : DateTime(22) Temp(10) Precip(11) Vent(11) Humid(10) Source(15)
hourlyWidths :: [Int]
hourlyWidths = [22, 10, 11, 11, 10, 15]

hourlyHeader :: Text
hourlyHeader = fmtCols hourlyWidths
  ["Date/Heure", "Temp(C)", "Precip(mm)", "Vent(m/s)", "Humid(%)", "Source"]

hourlyRow :: HourlyObs -> Text
hourlyRow o = fmtCols hourlyWidths
  [ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" (hoObservedAt o)
  , showM show (hoTemperature o)
  , showM show (hoPrecipitation o)
  , showM show (hoWindSpeed o)
  , showM show (hoHumidity o)
  , T.unpack (hoSource o)
  ]

-- Quotidien : Date(12) Tmin(10) Tmax(10) Precip(11) Soleil(11) Source(15)
dailyWidths :: [Int]
dailyWidths = [12, 10, 10, 11, 11, 15]

dailyHeader :: Text
dailyHeader = fmtCols dailyWidths
  ["Date", "Tmin(C)", "Tmax(C)", "Precip(mm)", "Soleil(mn)", "Source"]

dailyRow :: DailyObs -> Text
dailyRow o = fmtCols dailyWidths
  [ show (doObservedAt o)
  , showM show (doTMin o)
  , showM show (doTMax o)
  , showM show (doPrecipitation24h o)
  , showM show (doSunshineDuration o)
  , T.unpack (doSource o)
  ]

-- ---------------------------------------------------------------------------
-- Sortie JSON
-- ---------------------------------------------------------------------------

jStr :: String -> String
jStr s = "\"" ++ s ++ "\""

jM :: (a -> String) -> Maybe a -> String
jM f = maybe "null" f

stationToJson :: Station -> String
stationToJson s = "{" ++ intercalate ","
  [ "\"station_id\":"   ++ jStr (T.unpack (stationId s))
  , "\"name\":"         ++ jStr (T.unpack (stationName s))
  , "\"latitude\":"     ++ show (latitude s)
  , "\"longitude\":"    ++ show (longitude s)
  , "\"altitude\":"     ++ show (altitude s)
  , "\"opening_date\":" ++ jStr (show (openingDate s))
  , "\"closing_date\":" ++ jM (jStr . show) (closingDate s)
  ] ++ "}"

hourlyToJson :: HourlyObs -> String
hourlyToJson o = "{" ++ intercalate ","
  [ "\"station_id\":"    ++ jStr (T.unpack (hoStationId o))
  , "\"observed_at\":"   ++ jStr (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (hoObservedAt o))
  , "\"temperature\":"   ++ jM show (hoTemperature o)
  , "\"precipitation\":" ++ jM show (hoPrecipitation o)
  , "\"wind_speed\":"    ++ jM show (hoWindSpeed o)
  , "\"humidity\":"      ++ jM show (hoHumidity o)
  , "\"source\":"        ++ jStr (T.unpack (hoSource o))
  ] ++ "}"

dailyToJson :: DailyObs -> String
dailyToJson o = "{" ++ intercalate ","
  [ "\"station_id\":"        ++ jStr (T.unpack (doStationId o))
  , "\"observed_at\":"       ++ jStr (show (doObservedAt o))
  , "\"t_min\":"             ++ jM show (doTMin o)
  , "\"t_max\":"             ++ jM show (doTMax o)
  , "\"precipitation_24h\":" ++ jM show (doPrecipitation24h o)
  , "\"sunshine_duration\":" ++ jM show (doSunshineDuration o)
  , "\"source\":"            ++ jStr (T.unpack (doSource o))
  ] ++ "}"

printJson :: (a -> String) -> [a] -> IO ()
printJson toJson rows =
  putStrLn $ "[" ++ intercalate "," (map toJson rows) ++ "]"

-- ---------------------------------------------------------------------------
-- optparse-applicative
-- ---------------------------------------------------------------------------

data GlobalOpts = GlobalOpts
  { optDb   :: FilePath
  , optJson :: Bool
  , optCmd  :: Command
  }

data Command
  = CmdInit
  | CmdHistory
  | CmdUpdate
  | CmdStations (Maybe StationStatus)
  | CmdHourly   Text Day Day
  | CmdDaily    Text Day Day
  | CmdGeo      Double Double Double
  | CmdFetch    Text Day Day Frequency
  | CmdOrder    Text Day Day Frequency
  | CmdDownload Text

parseDay :: ReadM Day
parseDay = maybeReader (parseTimeM True defaultTimeLocale "%Y-%m-%d")

dayArg :: String -> Parser Day
dayArg mv = argument parseDay (metavar mv <> help "Format YYYY-MM-DD")

sidArg :: Parser Text
sidArg = T.pack <$> argument str
  (metavar "STATION-ID" <> help "Identifiant station (ex: 75114001)")

freqArg :: Parser Frequency
freqArg = argument (maybeReader go)
  (metavar "FREQUENCE" <> help "horaire | quotidienne")
  where
    go "horaire"     = Just Horaire
    go "quotidienne" = Just Quotidienne
    go _             = Nothing

subCmd :: String -> Parser Command -> String -> Mod CommandFields Command
subCmd name p desc = command name (info (p <**> helper) (progDesc desc))

commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ subCmd "init"    (pure CmdInit)    "Initialiser la base SQLite"
  , subCmd "history" (pure CmdHistory) "Charger l'historique depuis data.gouv.fr"
  , subCmd "update"  (pure CmdUpdate)  "Mettre à jour toutes les stations"
  , subCmd "stations" stationsP        "Lister les stations en base"
  , subCmd "hourly"   hourlyP          "Observations horaires d'une station"
  , subCmd "daily"    dailyP           "Observations quotidiennes d'une station"
  , subCmd "geo"      geoP             "Stations dans un rayon GPS (km)"
  , subCmd "fetch"    fetchP           "Commande + téléchargement + affichage (cycle complet)"
  , subCmd "order"    orderP           "Placer une commande → affiche le CommandeId sur stdout"
  , subCmd "download" downloadP        "Télécharger le résultat d'une commande"
  ]
  where
    stationsP = CmdStations <$> optional
      (   flag' Active (long "actives" <> help "Stations actives seulement")
      <|> flag' Closed (long "fermees" <> help "Stations fermées seulement"))

    hourlyP = CmdHourly <$> sidArg <*> dayArg "DEBUT" <*> dayArg "FIN"
    dailyP  = CmdDaily  <$> sidArg <*> dayArg "DEBUT" <*> dayArg "FIN"

    geoP = CmdGeo
      <$> argument auto (metavar "LAT"   <> help "Latitude centre (degrés)")
      <*> argument auto (metavar "LON"   <> help "Longitude centre (degrés)")
      <*> argument auto (metavar "RAYON" <> help "Rayon en kilomètres")

    fetchP = CmdFetch <$> sidArg <*> dayArg "DEBUT" <*> dayArg "FIN" <*> freqArg
    orderP = CmdOrder <$> sidArg <*> dayArg "DEBUT" <*> dayArg "FIN" <*> freqArg

    downloadP = CmdDownload . T.pack <$> argument str
      (metavar "COMMANDE-ID" <> help "Id retourné par 'order'")

globalParser :: Parser GlobalOpts
globalParser = GlobalOpts
  <$> strOption
        (long "db" <> metavar "PATH" <> value "meteo.db" <> showDefault
        <> help "Chemin vers la base SQLite")
  <*> switch (long "json" <> help "Sortie JSON brut (compatible jq)")
  <*> commandParser

examplesDoc :: Doc
examplesDoc = vsep $ map (pretty :: String -> Doc)
  [ "Exemples :"
  , ""
  , "  Initialisation :"
  , "    meteofrance-cli init"
  , "    meteofrance-cli history"
  , ""
  , "  Requetes en base (TUI scrollable, q pour quitter) :"
  , "    meteofrance-cli stations --actives"
  , "    meteofrance-cli hourly 75114001 2024-01-01 2024-01-31"
  , "    meteofrance-cli geo 48.85 2.35 50"
  , ""
  , "  Cycle API complet :"
  , "    meteofrance-cli fetch 75114001 2024-01-01 2024-01-03 horaire"
  , ""
  , "  Appels chaines (debug) :"
  , "    ID=$(meteofrance-cli order 75114001 2024-01-01 2024-01-02 horaire)"
  , "    meteofrance-cli download $ID"
  , ""
  , "  Sortie JSON pour piping :"
  , "    meteofrance-cli --json hourly 75114001 2024-01-01 2024-01-31 | jq '.[] | .temperature'"
  , "    meteofrance-cli --json stations | jq '[.[] | select(.closing_date == null)]'"
  ]

cliOpts :: ParserInfo GlobalOpts
cliOpts = info (globalParser <**> helper)
  ( fullDesc
  <> progDesc "Client CLI pour l'API Meteo-France DPClim et la base SQLite locale"
  <> header   "meteofrance-cli - client DPClim"
  <> footerDoc (Just examplesDoc)
  )

-- ---------------------------------------------------------------------------
-- Configuration API
-- ---------------------------------------------------------------------------

loadConfig :: IO ApiConfig
loadConfig = do
  mKey <- lookupEnv "METEO_API_KEY"
  case mKey of
    Nothing -> hPutStrLn stderr
      "[CLI] AVERTISSEMENT: METEO_API_KEY non défini — commandes API inopérantes"
    Just _  -> return ()
  return ApiConfig
    { apiKey          = T.pack (fromMaybe "" mKey)
    , apiBaseUrl      = "https://public-api.meteofrance.fr/public/DPClim/v1"
    , apiInitialWait  = 5
    , apiPollInterval = 3
    , apiMaxRetries   = 20
    }

-- ---------------------------------------------------------------------------
-- Exécution des commandes
-- ---------------------------------------------------------------------------

dayToUtc :: Day -> UTCTime
dayToUtc d = UTCTime d 0

runCommand :: Bool -> ApiConfig -> Connection -> Command -> IO ()

runCommand _ _ _ CmdInit =
  putStrLn "[CLI] Base SQLite initialisée."

runCommand _ _ conn CmdHistory = do
  putStrLn "[CLI] Chargement historique data.gouv.fr..."
  loadHistory conn

runCommand _ cfg conn CmdUpdate = do
  putStrLn "[CLI] Mise à jour en cours..."
  updateRecent cfg conn

runCommand json _ conn (CmdStations mStatus) = do
  stations <- getStations conn mStatus
  if json
    then printJson stationToJson stations
    else runTui "Stations" stationHeader (map stationRow stations)

runCommand json _ conn (CmdHourly sid d1 d2) = do
  obs <- getHourlyObs conn sid d1 d2
  if json
    then printJson hourlyToJson obs
    else runTui ("Horaire — " <> sid) hourlyHeader (map hourlyRow obs)

runCommand json _ conn (CmdDaily sid d1 d2) = do
  obs <- getDailyObs conn sid d1 d2
  if json
    then printJson dailyToJson obs
    else runTui ("Quotidien — " <> sid) dailyHeader (map dailyRow obs)

runCommand json _ conn (CmdGeo lat lon rad) = do
  stations <- geoSearch conn lat lon rad
  if json
    then printJson stationToJson stations
    else runTui (T.pack $ "Stations < " ++ show rad ++ " km") stationHeader
           (map stationRow stations)

runCommand json cfg _ (CmdFetch sid d1 d2 freq) = do
  let start = dayToUtc d1
      end   = dayToUtc d2
  hPutStrLn stderr $ "[CLI] Commande " ++ show freq ++ " pour " ++ T.unpack sid
  cmdId <- placeOrder cfg sid start end freq
  hPutStrLn stderr $ "[CLI] CommandeId: " ++ T.unpack (unCommandeId cmdId)
  hPutStrLn stderr "[CLI] Polling..."
  result <- downloadFile cfg cmdId
  case result of
    Left err -> hPutStrLn stderr $ "[CLI] Erreur: " ++ show err
    Right csv -> case freq of
      Horaire -> case parseHourlyCSV csv of
        Left  e   -> hPutStrLn stderr $ "[CLI] Parse erreur: " ++ e
        Right obs -> if json
          then printJson hourlyToJson obs
          else runTui ("Horaire — " <> sid) hourlyHeader (map hourlyRow obs)
      Quotidienne -> case parseDailyCSV csv of
        Left  e   -> hPutStrLn stderr $ "[CLI] Parse erreur: " ++ e
        Right obs -> if json
          then printJson dailyToJson obs
          else runTui ("Quotidien — " <> sid) dailyHeader (map dailyRow obs)

-- `order` : seul le CommandeId sort sur stdout (pour $(...))
-- les logs vont sur stderr
runCommand _ cfg _ (CmdOrder sid d1 d2 freq) = do
  let start = dayToUtc d1
      end   = dayToUtc d2
  hPutStrLn stderr $ "[CLI] Commande " ++ show freq ++ " pour " ++ T.unpack sid
  cmdId <- placeOrder cfg sid start end freq
  putStrLn $ T.unpack (unCommandeId cmdId)

runCommand json cfg _ (CmdDownload cid) = do
  hPutStrLn stderr "[CLI] Polling..."
  result <- downloadFile cfg (CommandeId cid)
  case result of
    Left err -> hPutStrLn stderr $ "[CLI] Erreur: " ++ show err
    Right csv ->
      -- Heuristique : essai horaire en premier, puis quotidien
      case parseHourlyCSV csv of
        Right obs@(_:_) -> if json
          then printJson hourlyToJson obs
          else runTui "Téléchargement" hourlyHeader (map hourlyRow obs)
        _ -> case parseDailyCSV csv of
          Right obs -> if json
            then printJson dailyToJson obs
            else runTui "Téléchargement" dailyHeader (map dailyRow obs)
          Left e -> hPutStrLn stderr $ "[CLI] Parse erreur: " ++ e

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  GlobalOpts dbPath json cmd <- execParser cliOpts
  cfg  <- loadConfig
  conn <- initDb dbPath
  runCommand json cfg conn cmd
