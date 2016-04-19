module Apply where
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

-- Apply the command to the server state and receive an output
apply :: ServerState -> Command -> IO ServerOut

-- Store data in the state
apply !state !(Store !path !dat) = do
  atomically $! do
    !currState <- readTVar state
    let !newPage      = Page path dat deathCounter False
        !updatedPages = HM.insert path newPage $! _pages currState
    modifyTVar' state $! \s ->
      set pages updatedPages s
  return $! DataSaved path

-- View data from the state. Check on harddisk if not in state
apply !state !(View !path) = do
  !currState <- readTVarIO state
  let !reqData = HM.lookup path $! _pages currState
  if isNothing reqData
    then do
      !myData <- findData path
      addDataToState state WellDone path $! myData
    else do
      -- reset cache timeout
      resetCacheTimeout state path
      return $! ViewData path $! _pageData $! fromJust reqData

-- View raw data from the state. Check on harddisk if not in state
apply !state !(ViewRaw !path) = do
  !currState <- readTVarIO state
  let !reqData = HM.lookup path $! _pages currState
  if isNothing reqData
    then do
      !myData <- findData path
      addDataToState state Raw path $! myData
    else do
      -- reset cache timeout
      resetCacheTimeout state path
      return $! ViewRawData path $! _pageData $! fromJust reqData

{-# INLINABLE apply #-}
