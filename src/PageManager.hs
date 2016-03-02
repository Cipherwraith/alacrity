module PageManager where
import Types
import Config
import Helpers

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- This will oversee the pages stored in memory
--   1. Remove all data with _timeToDie less than or equal to 0
--   2. Sleep until needed again (default 1 second)
--   Note: 
--    Depending on how many pages are in the state there is no guarantee that 
--    the manager will run promptly every interval, but it will never be faster
--    than said interval.
pageManager :: TVar State -> IO ()
pageManager state = do
  atomically $! do
    cState <- readTVar state
    let cState' = HM.mapMaybe removeDeadPages $! _pages cState
    modifyTVar' state $! \(s) -> do
      State cState'
  threadDelay interval
  pageManager state

-- Returns `Nothing` if a page has been removed from hashmap
-- Returns `Just Page` if a page is kept in the map. 
--   * Decrements _timeToDie
removeDeadPages :: Page -> Maybe Page
removeDeadPages p 
  | (_timeToDie p) > 0 = Just $ decPage p
  | otherwise = Nothing

-- Subtracts one second from Page timeToDie
decPage :: Page -> Page
decPage p = p { _timeToDie = ((_timeToDie p) - 1) }
