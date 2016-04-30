module WebApp where
import Config
import Types
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Network.Wai.Internal
import Network.Wai.Parse
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B hiding (pack,unpack)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Apply
import Helpers
import Data.Aeson ((.:), (.=), (.:?), decode, encode, ToJSON(..), object, FromJSON(..), Value(..), decodeStrict)
import System.FilePath.Posix
import Network.HTTP.Types.Status
import Safe
import qualified Data.HashMap.Strict as HM

serveWeb :: Int -> ServerState -> IO ()
serveWeb !port !state = scotty port $! do
  middleware $! gzip $! def { gzipFiles = GzipCompress }
  serveRoutes state
{-# INLINABLE serveWeb #-}

invalidPassword :: ActionM ()
invalidPassword = do
  status forbidden403
  text "NG credentials"
{-# INLINE invalidPassword #-}

noData :: ActionM ()
noData = do
  status forbidden403
  text "NG no data"
{-# INLINE noData #-}

processFiles :: ServerState -> FilePath -> B.ByteString -> ActionM ()
processFiles !state !loc !file = do
  -- liftIO $! putStrLn "                              > Post"
  let !cmd = processFile file loc
  !result <- liftIO $! state `apply` cmd
  let !prepped = rawData result
      !ext = takeExtension $! makePath indexSettings loc
  !o <- outputResult ext prepped
  return $! o
  
{-# INLINE processFiles #-}

processFile :: B.ByteString -> FilePath -> Command
processFile !file !loc = Store loc file
{-# INLINE processFile #-}

serveRoutes :: ServerState -> ScottyM ()
serveRoutes !state = do

  -- Grabs POSTs to /_write and saves them to alacrity
  post "/_write" $! do
    p <- param "password"
    l <- param "loc" 
    f <- param "page"
    if passwordIsCorrect p && (l /= (""::String))
      then processFiles state l f
      else invalidPassword

  -- Grabs POSTs to /_write and saves them to alacrity
  post "/_monitor" $! do
    p <- param "password"
    if passwordIsCorrect p 
      then do
        !monitor <- liftIO $! getMonitor state
        text $! TL.fromStrict $! monitor
      else invalidPassword

  -- Matches any GET request and sends to alacrity backend
  get (function $! \req -> Just [("foo","bar")]) $! do
    !req <- request
    let !path = T.unpack . T.intercalate "/" . pathInfo $! req
        !cmd = ViewRaw path
        !ext = takeExtension $! makePath indexSettings path
    !result <- liftIO $! state `apply` cmd
    let !prepped = rawData result
    !o <- outputResult ext prepped
    return $! o
  
  -- 404 Not found. Matches all non `GET`
  notFound $! text "[ Alacrity ] 404: Not Found"
{-# INLINABLE serveRoutes #-}

-- Takes the output data and outputs it with the appropriate content-type
outputResult :: String -> (B.ByteString, Status) -> ActionM ()
outputResult !ext !(!dat, !httpStatus) = outResult httpStatus dat $! 
    case ext of
      ".html" -> customOut HTML
      ".json" -> customOut JSON
      ".css"  -> customOut CSS
      ".jpg"  -> customOut JPG
      ".gif"  -> customOut GIF
      ".mp4"  -> customOut MP4
      ".ogg"  -> customOut OGG
      ".ico"  -> customOut ICO
      ".svg"  -> customOut SVG
      ".png"  -> customOut PNG
      ".pdf"  -> customOut PDF
      ".jpeg" -> customOut JPEG
      ".webm" -> customOut WEBM
      ".js"   -> customOut JS
      ".txt"  -> customOut TXT
      _       -> customOut Unsupported
{-# INLINABLE outputResult #-}

-- Manages the output content type and binary/text implementations
--  Natively supported file types: 
--    txt jpg webm html json gif jpeg php png ico svg css js
--        (defaults to octet-stream for non-supported file types)
customOut :: CustomOut -> BL.ByteString -> ActionM ()
customOut !cout !pFile = do
  case cout of
    HTML        -> setContentType "text/html; charset=utf-8"
    JSON        -> setContentType "application/json; charset=utf-8"
    CSS         -> setContentType "text/css; charset=utf-8"
    GIF         -> setContentType "image/gif"
    SVG         -> setContentType "image/svg+xml"
    ICO         -> setContentType "image/x-icon"
    JPG         -> setContentType "image/jpeg"
    MP4         -> setContentType "video/mp4"
    OGG         -> setContentType "audio/ogg"
    PNG         -> setContentType "image/png"
    PDF         -> setContentType "application/pdf"
    JPEG        -> setContentType "image/jpeg"
    WEBM        -> setContentType "video/webm"
    JS          -> setContentType "text/javascript; charset=utf-8"
    TXT         -> setContentType "text/plain; charset=utf-8"
    Unsupported -> setContentType "application/octet-stream"
  !r <- raw pFile
  return $! r
{-# INLINABLE customOut #-}

-- Set the content type of the output
setContentType :: TL.Text -> ActionM()
setContentType !s = setHeader "Content-Type" $! s
{-# INLINABLE setContentType #-}

-- Outputs the result using the specified http status code and content type
outResult :: Status -> B.ByteString -> (BL.ByteString -> ActionM ()) -> ActionM ()
outResult !s !d !o = do
  status $! s
  !r <- o . BL.fromStrict $! d
  return $! r
{-# INLINABLE outResult #-}

