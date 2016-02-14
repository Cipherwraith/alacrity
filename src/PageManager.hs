module PageManager where
import Types

import Control.Concurrent.STM.TVar

pageManager :: TVar State -> IO ()
pageManager = undefined
