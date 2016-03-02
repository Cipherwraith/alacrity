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
-- import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Helpers
import Data.Either

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

-- Apply the command to the server state and receive an output
apply :: ServerState -> Command -> IO ServerOut

-- Store data in the state
apply state (Store path dat) = do
  atomically $ do
    currState <- readTVar state
    let newPage      = Page path dat deathCounter False
        updatedPages = HM.insert path newPage (_pages currState)
    modifyTVar' state $ \s -> 
      set pages updatedPages s
  return $! DataSaved path

-- View data from the state. Check on harddisk if not in state
apply state (View path) = do
  currState <- readTVarIO state
  let reqData = HM.lookup path $! _pages currState
  if isNothing reqData
    then do
      myData <- findData path
      addDataToState state path myData
    else do
      -- reset cache timeout
      resetCacheTimeout state path
      return $! ViewData path (_pageData $! fromJust reqData)

resetCacheTimeout :: ServerState -> FilePath -> IO ()
resetCacheTimeout state fp = do
  atomically $ do
    cState <- readTVar state
    let updatedPages = HM.adjust resetTimeout fp (view pages cState)
    modifyTVar' state $ \s -> do
      set pages updatedPages s

resetTimeout :: Page -> Page
resetTimeout p = set timeToDie deathCounter p

addDataToState :: ServerState -> FilePath -> Either ErrorText FileData -> IO ServerOut
addDataToState _ _ (Left err) = return $ ErrorMsg err
addDataToState state path (Right dat) = do
  atomically $ do
    cState <- readTVar state
    let newPage      = Page path dat deathCounter True
        updatedPages = HM.insert path newPage (_pages cState)
    modifyTVar' state $ \s ->
      set pages updatedPages s
    return $! ViewData path dat
  

