module Helpers where
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B hiding (unpack, pack)
import qualified Data.ByteString.Char8 as B (unpack, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Config
import Types
import System.FilePath.Posix
import Development.Shake.FilePath
--import qualified System.Path.Directory as Dir
--import qualified System.Path.IO as PathIO
--import qualified System.Path as Path
--import System.Path ((</>))
import System.Directory 
import qualified Network.WebSockets as WS
import Data.Aeson ((.:), (.=), (.:?), decode, encode, ToJSON(..), object, FromJSON(..), Value(..), decodeStrict)
import Data.Monoid
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Types.Status

-- Helper that organizes the absolute path of file
makePath :: IndexSettings -> FilePath -> FilePath
makePath (IndexSettings False _ ) fp = rakedPath fp
makePath (IndexSettings True indexAss ) fp 
  | fp == ""                = rakedPath indexAss
  | (takeFileName fp) == "" = rakedPath (fp <> indexAss)
  | otherwise               = rakedPath fp

-- Function that builds the absolute path, taking into account the server root
rakedPath :: FilePath -> FilePath
rakedPath !fp = makeValid . normalise . (<>) (serverRoot <> "/") . mconcat . filter (not . (==) "../") . splitPath $! fp


-- Write data to harddisk from cache
writeData :: FilePath -> B.ByteString -> IO ()
writeData path dat = do
  let myPath = makePath indexSettings path
  createDirectoryIfMissing True $! takeDirectory myPath
  B.writeFile myPath dat

-- Find file data on the harddisk if its not in cache
findData :: FilePath -> IO (Either ErrorText FileData)
findData path = do
  let myPath = makePath indexSettings path
  fileExistence <- doesFileExist myPath
  if fileExistence
    then do
      myData <- B.readFile myPath
      return $! Right myData
    else 
      return $! Left "e0001" -- "error: file does not exist"

prepByteString :: BL.ByteString -> T.Text
prepByteString = T.decodeUtf8 . BL.toStrict

-- Responds to a connection with a message
respond :: WS.WebSocketsData a => WS.Connection -> a -> IO ()
respond = WS.sendTextData

-- Decode a proper json msg into native haskell data type
decodeMsg :: T.Text -> Maybe Msg
decodeMsg = decodeStrict . T.encodeUtf8

addDataToState :: ServerState -> Rawness -> FilePath -> Either ErrorText FileData -> IO ServerOut
addDataToState _ _ _ (Left err) = return $ ErrorMsg err
addDataToState state rawness path (Right dat) = do
  atomically $ do
    cState <- readTVar state
    let newPage      = Page path dat deathCounter True
        updatedPages = HM.insert path newPage (_pages cState)
    modifyTVar' state $ \s ->
      set pages updatedPages s
    case rawness of
      Raw      -> return $! ViewRawData path dat
      WellDone -> return $! ViewData    path dat

resetCacheTimeout :: ServerState -> FilePath -> IO ()
resetCacheTimeout state fp = do
  atomically $ do
    cState <- readTVar state
    let updatedPages = HM.adjust resetTimeout fp (view pages cState)
    modifyTVar' state $ \s -> do
      set pages updatedPages s

resetTimeout :: Page -> Page
resetTimeout p = set timeToDie deathCounter p

rawData :: ServerOut -> (B.ByteString, Status)
rawData (ErrorMsg s) = case s of
  "e0001" -> (B.pack s, notFound404)
  "e0002" -> (B.pack s, badRequest400)
  "e0003" -> (B.pack s, unprocessable422)
  "e0004" -> (B.pack s, notImplemented501)
rawData (DataSaved s) = (B.pack s, created201)
rawData (ViewData _ s) = (s, ok200)
rawData (ViewRawData _ s) = (s, ok200)

--binaryToBase64 :: B.ByteString -> T.Text
--binaryToBase64 b = T.pack . B.unpack . B64.encode $ b

unprocessable422 :: Status
unprocessable422 = mkStatus 422 "Unprocessable Entity"
