module WebApp where
import Config
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
import Network.HTTP.Types.Status

serveWeb :: Int -> ServerState -> IO ()
serveWeb port state = scotty port $! do
  middleware $! gzip $! def { gzipFiles = GzipCompress }
  serveRoutes state

serveRoutes :: ServerState -> ScottyM ()
serveRoutes !state = do

  -- Matches any GET request and sends to alacrity backend
  get (function $ \req -> Just [("foo","bar")]) $! do
    req <- request
    let path = T.unpack . T.intercalate "/" . pathInfo $ req
        cmd = ViewRaw path
        ext = takeExtension $! makePath indexSettings path
    result <- liftIO $! state `apply` cmd
    let prepped = rawData result
    outputResult ext prepped

  -- 404 Not found. Matches all non `GET`
  notFound $! text "[ Alacrity ] 404: Not Found"

-- Takes the output data and outputs it with the appropriate content-type
outputResult :: String -> (T.Text, Status) -> ActionM ()
outputResult ext (dat, httpStatus) = outResult httpStatus dat $! 
    case ext of
      ".html" -> html
      ".json" -> json
      ".css"  -> customOut CSS
      ".js"   -> customOut JS
      _       -> text

customOut :: CustomOut -> TL.Text -> ActionM ()
customOut CSS t = do
  setHeader "Content-Type" "text/css; charset=utf-8"
  text t

customOut JS t = do
  setHeader "Content-Type" "text/css; charset=utf-8"
  text t


-- Outputs the result using the specified http status code and content type
outResult :: Status -> T.Text -> (TL.Text -> ActionM ()) -> ActionM ()
outResult s d o = do
  status s
  o . TL.fromStrict $! d

