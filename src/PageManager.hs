module PageManager where
import Types
import Config
import Helpers

import Control.Lens
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (void)

-- This will oversee the pages stored in memory
--   1. Remove all data with _timeToDie less than or equal to 0
--   2. Sleep until needed again (default 1 second)
--   Note: 
--    Depending on how many pages are in the state there is no guarantee that 
--    the manager will run promptly every interval, but it will never be faster
--    than said interval.
pageManager :: ServerState -> IO ()
pageManager state = do
  lossyWritePages state
  managePages state
  threadDelay interval
  pageManager state

-- This is a `lossy` command because it has a chance to miss some files.
-- This shouldnt be a problem because the next tick will get the files.
-- The newest file will be available in the cache regardless of a missed write
lossyWritePages :: ServerState -> IO ()
lossyWritePages state = do
  cState <- readTVarIO state
  let pages          = _pages cState
      pagesNotOnDisk = HM.elems $! checkDisk pages
  pagesWritten <- mapM writePageToDisk pagesNotOnDisk
  mapM_ (setHardDiskFlag state) pagesWritten

-- Sets the "on hard disk" flag as true for all recently written files
setHardDiskFlag :: ServerState -> FilePath -> IO ()
setHardDiskFlag state fp = atomically $ do
  cState <- readTVar state
  let updatedPages = HM.adjust setHDFlag fp (view pages cState)
  modifyTVar' state $ \s -> do
    set pages updatedPages s

-- Sets the "on hard disk" flag as true
setHDFlag :: Page -> Page
setHDFlag p = set onHarddisk True p

-- writes the page to disk, discarding any return data, then returns filepath
writePageToDisk :: Page -> IO FilePath
writePageToDisk p = do
  _ <- void $! writeData (view pagePath p) (view pageData p) 
  return $! view pagePath p

-- Returns a hashmap of all the pages that arent already written to harddisk
checkDisk :: HM.HashMap FilePath Page -> HM.HashMap FilePath Page
checkDisk hm = HM.filter (not . onDiskFilter) hm

-- Returns True if the Page is already written to disk
onDiskFilter :: Page -> Bool
onDiskFilter p = view onHarddisk p

-- Removes dead pages and decrements the cache timer
managePages :: ServerState -> IO ()
managePages state = do
  atomically $! do
    cState <- readTVar state
    let cState' = HM.mapMaybe removeDeadPages $! _pages cState
    modifyTVar' state $ \s -> State cState'

-- Returns `Nothing` if a page has been removed from hashmap
-- Returns `Just Page` if a page is kept in the map. 
--   * Decrements _timeToDie
removeDeadPages :: Page -> Maybe Page
removeDeadPages p 
  | _timeToDie p > 0 = Just $ decPage p
  | otherwise = Nothing

-- Subtracts one second from Page timeToDie
decPage :: Page -> Page
decPage p = p { _timeToDie = _timeToDie p - 1 }
