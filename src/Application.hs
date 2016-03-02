module Application where
import Types
import qualified Network.WebSockets as WS
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.Aeson ((.:), (.=), (.:?), decode, encode, ToJSON(..), object, FromJSON(..), Value(..), decodeStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Control.Lens
import Config
import System.FilePath ((</>))
import System.Directory (doesFileExist)

application :: ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  forever (converse state conn)

converse :: ServerState -> WS.Connection -> IO ()
converse state conn = do
  msg <- WS.receiveData conn :: IO T.Text
  let req = parseMsg msg
  case req of
    Left errCode -> report conn $ ErrorMsg errCode
    Right cmd -> do
      result <- state `apply` cmd
      report conn result

report :: WS.Connection -> ServerOut -> IO ()
report conn serverOut = do
  let output = prepByteString $ encode serverOut
  respond conn output

prepByteString :: BL.ByteString -> T.Text
prepByteString = T.decodeUtf8 . BL.toStrict

parseMsg :: T.Text -> Either ErrorText Command
parseMsg msg = case decodeMsg msg of
  Nothing -> Left "e0002" -- "error: cant decode message"
  Just cmd -> parseCommand cmd

parseCommand :: Msg -> Either ErrorText Command
parseCommand (Msg cmd path dat) 
  | cmd == "store" = 
      case dat of
        Nothing   -> Left "e0003" -- "error: no data received"
        Just dat' -> Right $! Store path dat'
  | cmd == "view"  = Right $ View path
  | otherwise      = Left "e0004" -- "error: couldnt parse command"

-- Responds to a connection with a message
respond :: WS.Connection -> T.Text -> IO ()
respond c t = WS.sendTextData c t

-- Decode a proper json msg into native haskell data type
decodeMsg :: T.Text -> Maybe Msg
decodeMsg = decodeStrict . T.encodeUtf8

-- Apply the command to the server state and receive an output
apply :: ServerState -> Command -> IO ServerOut

-- Store data in the state
apply state (Store path dat) = do
  atomically $ do
    currState <- readTVar state
    let newPage      = Page path dat deathCounter False
        updatedPages = HM.insert path newPage (_pages currState)
    modifyTVar' state $ \(s) -> do
      set pages updatedPages s
  return $! DataSaved path

-- View data from the state
--   modify later to also check harddisk if not stored in state
apply state (View path) = do
  currState <- readTVarIO state
  let reqData = HM.lookup path $! _pages currState
  if (isNothing reqData)
    then do
      myData <- findData path
      return $! myData
    else return $! ViewData path (_data $! fromJust reqData)

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
  
writeData :: FilePath -> T.Text -> IO ()
writeData path dat = do
  let myPath = serverRoot </> path
  undefined

