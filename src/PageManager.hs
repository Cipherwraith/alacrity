module PageManager where
import Types
import Config
import Helpers

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Data.Monoid
import Data.Maybe

-- This will oversee the pages stored in memory
--   1. Remove all data with _timeToDie less than or equal to 0
--   2. Sleep until needed again (default 1 second)
--   Note: 
--    Depending on how many pages are in the state there is no guarantee that 
--    the manager will run promptly every interval, but it will never be faster
--    than said interval.
pageManager :: ServerState -> IO ()
pageManager !state = do
  !l <- lossyWritePages state
  !m <- managePages state
  !d <- wait
  return $! d
{-# INLINABLE pageManager #-}

wait :: IO ()
wait = do
  liftIO $! putStrLn "                              * Waiting"
  !d <- threadDelay interval
  return $! d

-- This is a `lossy` command because it has a chance to miss some files.
-- This shouldnt be a problem because the next tick will get the files.
-- The newest file will be available in the cache regardless of a missed write
lossyWritePages :: ServerState -> IO ()
lossyWritePages !state = do
  !cState <- readTVarIO state
  liftIO $! putStrLn "                              * Writing Pages"
  let !myPages        = view pages $! cState
      !pagesNotOnDisk = HM.elems $! checkDisk myPages
      !pageCount      = length pagesNotOnDisk
  !pagesWritten <- mapM writePageToDisk pagesNotOnDisk
  !m <- mapM_ (setHardDiskFlag state) $! catMaybes $! pagesWritten
  liftIO $! putStrLn $! "                              * Cached: " <> show pageCount
  return $! m
{-# INLINABLE lossyWritePages #-}

-- Sets the "on hard disk" flag as true for all recently written files
setHardDiskFlag :: ServerState -> FilePath -> IO ()
setHardDiskFlag !state !fp = atomically $! do
  !cState <- readTVar state
  let !updatedPages = HM.adjust setHDFlag fp (view pages cState)
  modifyTVar' state $! \s -> do
    set pages updatedPages s
{-# INLINABLE setHardDiskFlag #-}

-- Sets the "on hard disk" flag as true
setHDFlag :: Page -> Page
setHDFlag !p = set onHarddisk True $! p
{-# INLINABLE setHDFlag #-}

-- writes the page to disk, discarding any return data, then returns filepath
writePageToDisk :: Page -> IO (Maybe FilePath)
writePageToDisk !p = do
  let !timeLeft = _timeToDie p
  if (timeLeft == 1) 
    then do
      !w <- writeData (view pagePath p) $! view pageData p
      return $! Just $! view pagePath p
    else do
      return $! Nothing
{-# INLINABLE writePageToDisk #-}

-- Returns a hashmap of all the pages that arent already written to harddisk
checkDisk :: HM.HashMap FilePath Page -> HM.HashMap FilePath Page
checkDisk !hm = HM.filter (not . onDiskFilter) $! hm
{-# INLINABLE checkDisk #-}

-- Returns True if the Page is already written to disk
onDiskFilter :: Page -> Bool
onDiskFilter !p = view onHarddisk $! p
{-# INLINABLE onDiskFilter #-}

-- Removes dead pages and decrements the cache timer
managePages :: ServerState -> IO ()
managePages !state = do
  liftIO $! putStrLn "                              * Managing Pages"
  atomically $! do
    !cState <- readTVar state
    let !cState' = HM.mapMaybe removeDeadPages $! _pages cState
    modifyTVar' state $! \s -> State cState'
{-# INLINABLE managePages #-}

-- Returns `Nothing` if a page has been removed from hashmap
-- Returns `Just Page` if a page is kept in the map. 
--   * Decrements _timeToDie
removeDeadPages :: Page -> Maybe Page
removeDeadPages !p 
  | _timeToDie p > 0 = Just $! decPage p
  | otherwise = Nothing
{-# INLINABLE removeDeadPages #-}

-- Subtracts one second from Page timeToDie
decPage :: Page -> Page
decPage !p = p { _timeToDie = _timeToDie p - 1 }
{-# INLINABLE decPage #-}
