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
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL

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
  Nothing -> Left "error: cant decode message"
  Just cmd -> parseCommand cmd

parseCommand :: Msg -> Either ErrorText Command
parseCommand (Msg cmd path dat) 
  | cmd == "store" = 
      case dat of
        Nothing   -> Left "error: no data received"
        Just dat' -> Right $! Store path dat'
  | cmd == "view"  = Right $ View path
  | otherwise = Left "error: couldnt parse command"

-- Responds to a connection with a message
respond :: WS.Connection -> T.Text -> IO ()
respond c t = WS.sendTextData c t

decodeMsg = undefined
apply = undefined

