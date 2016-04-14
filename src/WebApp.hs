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
import qualified Data.ByteString as B
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
outputResult :: String -> (B.ByteString, Status) -> ActionM ()
outputResult ext (dat, httpStatus) = outResult httpStatus dat $! 
    case ext of
      ".html" -> customOut HTML
      ".json" -> customOut JSON
      ".css"  -> customOut CSS
      ".js"   -> customOut JS
      ".txt"  -> customOut TXT
      _       -> customOut Unsupported

-- Manages the output content type and binary/text implementations
--  Natively supported file types: 
--    txt jpg webm html json gif jpeg php png ico svg css js
--        (defaults to octet-stream for non-supported file types)
customOut :: CustomOut -> BL.ByteString -> ActionM ()
customOut cout pFile = do
  case cout of
    HTML        -> setContentType "text/html; charset=utf-8"
    JSON        -> setContentType "application/json; charset=utf-8"
    CSS         -> setContentType "text/css; charset=utf-8"
    JS          -> setContentType "text/javascript; charset=utf-8"
    TXT         -> setContentType "text/plain; charset=utf-8"
    Unsupported -> setContentType "application/octet-stream"
  raw pFile

-- Set the content type of the output
setContentType :: TL.Text -> ActionM()
setContentType s = setHeader "Content-Type" s

-- Outputs the result using the specified http status code and content type
outResult :: Status -> B.ByteString -> (BL.ByteString -> ActionM ()) -> ActionM ()
outResult s d o = do
  status s
  o . BL.fromStrict $! d

