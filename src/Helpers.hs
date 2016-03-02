module Helpers where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Config
import Types
import System.FilePath.Posix
--import qualified System.Path.Directory as Dir
--import qualified System.Path.IO as PathIO
--import qualified System.Path as Path
--import System.Path ((</>))
import System.Directory 
import qualified Network.WebSockets as WS
import Data.Aeson ((.:), (.=), (.:?), decode, encode, ToJSON(..), object, FromJSON(..), Value(..), decodeStrict)
import Data.Monoid


makePath :: FilePath -> FilePath
makePath fp = makeValid . normalise $! serverRoot <> fp

-- Write data to harddisk from cache
writeData :: FilePath -> T.Text -> IO ()
writeData path dat = do
  let myPath = makePath path
  createDirectoryIfMissing True $! takeDirectory myPath
  T.writeFile myPath dat

-- Find file data on the harddisk if its not in cache
findData :: FilePath -> IO (Either ErrorText FileData)
findData path = do
  let myPath = makePath path
  fileExistence <- doesFileExist myPath
  if fileExistence
    then do
      myData <- T.readFile myPath
      return $! Right myData
    else
      return $! Left "e0001" -- "error: file does not exist"

prepByteString :: BL.ByteString -> T.Text
prepByteString = T.decodeUtf8 . BL.toStrict

-- Responds to a connection with a message
respond :: WS.Connection -> T.Text -> IO ()
respond = WS.sendTextData 

-- Decode a proper json msg into native haskell data type
decodeMsg :: T.Text -> Maybe Msg
decodeMsg = decodeStrict . T.encodeUtf8
