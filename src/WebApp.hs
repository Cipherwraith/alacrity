module WebApp where
import Types
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Network.Wai.Internal
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Apply
import Helpers
import Data.Aeson ((.:), (.=), (.:?), decode, encode, ToJSON(..), object, FromJSON(..), Value(..), decodeStrict)

serveWeb :: Int -> ServerState -> IO ()
serveWeb port state = scotty port $! do
  middleware $! gzip $! def { gzipFiles = GzipCompress }
  serveRoutes state

serveRoutes :: ServerState -> ScottyM ()
serveRoutes !state = do
  notFound $! do
    req <- request
    let path = T.unpack . T.intercalate "/" . pathInfo $ req
        cmd = ViewRaw path
    result <- liftIO $! state `apply` cmd
    let prepped = prepByteString . encode $! result
    html . TL.fromStrict $! prepped
  
