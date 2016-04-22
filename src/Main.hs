module Main where

import Application
import PageManager
import Types
import Config
import WebApp

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.STM
import qualified Network.WebSockets as WS
import System.IO
import Web.Scotty
import System.Posix.User

-- Todo:
--    example ->
--      `./alacrity -p1001 -r/srv/http -d10 -t250000`
--      port 1001, root directory is /srv/http, 10 ticks to cache death, and 250 milliseconds per tick
--
--   - Options passed on startup -
--     * Make a way to define root path
--     * timeToDie for pages
--     * interval time
--     * Port number
--     * Whether or not to enable the http server

-- Set output encoding, start our server, then start our application
main :: IO ()
main = do
  hSetEncoding stdout utf8
  !pageState <- liftIO $! newTVarIO newState
  _ <- forkIO $! forever $! pageManager pageState
  forkIO $! WS.runServer "0.0.0.0" serverPort $! application pageState
  serveWeb 3333 pageState
