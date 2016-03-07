module WebApp where
import Types
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Network.Wai.Internal
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Apply
import Helpers
import Data.Aeson ((.:), (.=), (.:?), decode, encode, ToJSON(..), object, FromJSON(..), Value(..), decodeStrict)
import System.FilePath.Posix

serveWeb :: Int -> ServerState -> IO ()
serveWeb port state = scotty port $! do
  middleware $! gzip $! def { gzipFiles = GzipCompress }
  serveRoutes state

serveRoutes :: ServerState -> ScottyM ()
serveRoutes !state = do
  notFound $! do
    req <- request
    let path = T.unpack . T.intercalate "/" . pathInfo $ req
        cmd = ViewRaw path
        ext = takeExtension path
    result <- liftIO $! state `apply` cmd
    let prepped = rawData result
    outputResult ext prepped
    
outputResult :: String -> T.Text -> ActionM ()
outputResult ext dat =
  case ext of
    ".html" -> html . TL.fromStrict $! dat
    ".json" -> json . TL.fromStrict $! dat
    _       -> text . TL.fromStrict $! dat
