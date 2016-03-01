module Types where
import Control.Lens hiding ((.=))
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import Data.Aeson ((.:), (.=), (.:?), decode, encode, ToJSON(..), object, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- Empty state for initializing the server
newState :: State
newState = State HM.empty

type ServerState = TVar State

-- Data about our page stored in memory
data Page = Page { 
    _absolutePath :: FilePath -- Path where the file is stored
  , _data         :: FileData -- Data stored in the file; UTF8 format
  , _timeToDie    :: Int      -- Seconds until the data is removed from State cache
  , _onHarddisk   :: Bool     -- Whether or not this page has been saved to hard disk
  } deriving (Show)

-- Cache pages in the server state until their _timeToDie hits 0
data State = State {
    _pages :: HM.HashMap FilePath Page
  } deriving (Show)

-- Input data
type FileData = T.Text
type ErrorText = String

-- Server Output
data ServerOut = ErrorMsg String | DataSaved String | ViewData FilePath T.Text

instance ToJSON ServerOut where
  toJSON (ErrorMsg  errorMessage) = 
    object ["error" .= errorMessage]

  toJSON (DataSaved fileName) = 
    object ["saved" .= fileName]

  toJSON (ViewData fileName contents) = 
    object ["view" .= contents, "file" .= fileName]

-- Command list
data Command = Store FilePath T.Text | View FilePath

data Msg = Msg {
    _command :: String
  , _filepath :: FilePath
  , _filedata :: Maybe T.Text
}

instance FromJSON Msg where
  parseJSON (Object v) = 
    Msg <$>
    (v .: "command") <*>
    (v .: "path") <*>
    (v .:? "data") 


