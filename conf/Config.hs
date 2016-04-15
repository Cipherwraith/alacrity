module Config where
--import System.FilePath.Posix
--import Data.Monoid
import Types

-- Password needed for connecting to the websocket interface
socketPassword :: String
socketPassword = "any_password"

-- One second in microseconds
interval :: Int
interval = 1000000

-- port number
serverPort :: Int
serverPort = 1001

-- time to die constant in seconds. need to change to option later
deathCounter :: Int
deathCounter = 10

-- server document root
serverRoot :: FilePath 
serverRoot = "/srv/http"

-- look at `Types.hs` for more info about this setting
indexSettings :: IndexSettings
indexSettings = IndexSettings True "index.html"
