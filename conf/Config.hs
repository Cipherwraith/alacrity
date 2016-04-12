module Config where
--import System.FilePath.Posix
--import Data.Monoid

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
