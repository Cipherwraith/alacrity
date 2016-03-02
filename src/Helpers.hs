module Helpers where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Config
import Types
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import qualified Network.WebSockets as WS
import Data.Aeson ((.:), (.=), (.:?), decode, encode, ToJSON(..), object, FromJSON(..), Value(..), decodeStrict)

-- Write data to harddisk from cache
writeData :: FilePath -> T.Text -> IO ()
writeData path dat = do
  let myPath = serverRoot </> path
  undefined

-- Find file data on the harddisk if its not in cache
findData :: FilePath -> IO ServerOut
findData path = do
  let myPath = serverRoot </> path
  fileExistence <- doesFileExist myPath
  if fileExistence
    then do
      myData <- T.readFile $ serverRoot </> path
      return $! ViewData path myData
    else
      return $! ErrorMsg "e0001" -- "error: file does not exist"

prepByteString :: BL.ByteString -> T.Text
prepByteString = T.decodeUtf8 . BL.toStrict

-- Responds to a connection with a message
respond :: WS.Connection -> T.Text -> IO ()
respond c t = WS.sendTextData c t

-- Decode a proper json msg into native haskell data type
decodeMsg :: T.Text -> Maybe Msg
decodeMsg = decodeStrict . T.encodeUtf8
