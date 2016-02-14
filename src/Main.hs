module Main where

import Application
import PageManager
import Types

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Network.WebSockets as WS
import System.IO

-- Set output encoding, start our server, then start our application
main :: IO ()
main = do
  hSetEncoding stdout utf8
  pageState <- liftIO $ newTVarIO newState
  _ <- forkIO $! pageManager pageState
  WS.runServer "0.0.0.0" 4242 $! application pageState
