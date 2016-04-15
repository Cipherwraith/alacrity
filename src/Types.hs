module Types where
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B hiding (unpack, pack)
import qualified Data.ByteString.Char8 as B (unpack, pack)
import Control.Lens hiding ((.=))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Aeson ((.:), (.=), (.:?), decode, encode, ToJSON(..), object, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
--import Helpers

-- Empty state for initializing the server
newState :: State
newState = State HM.empty

type ServerState = TVar State

-- Data about our page stored in memory
data Page = Page { 
    _pagePath   :: FilePath -- Path where the file is stored
  , _pageData   :: FileData -- Data stored in the file
  , _timeToDie  :: Int      -- Seconds until the data is removed from State cache
  , _onHarddisk :: Bool     -- Whether or not this page has been saved to hard disk
  } deriving (Show)

-- Cache pages in the server state until their _timeToDie hits 0
data State = State {
    _pages :: HM.HashMap FilePath Page
  } deriving (Show)

-- Input data
type FileData = B.ByteString
type ErrorText = String

-- Server Output
data ServerOut = ErrorMsg String | DataSaved String | ViewData FilePath B.ByteString | ViewRawData FilePath B.ByteString

data Rawness = Raw | WellDone

instance ToJSON ServerOut where
  toJSON (ErrorMsg  errorMessage) = 
    object ["error" .= errorMessage]

  toJSON (DataSaved fileName) = 
    object ["saved" .= fileName]

  toJSON (ViewData fileName contents) = 
    object ["view" .= (binaryToBase64 contents), "file" .= fileName]

  toJSON (ViewRawData _ contents) = String . binaryToBase64 $ contents

binaryToBase64 :: B.ByteString -> T.Text
binaryToBase64 b = T.pack . B.unpack . B64.encode $ b

base64ToBinary :: T.Text -> B.ByteString
base64ToBinary t = B64.decodeLenient . B.pack . T.unpack $ t

-- Command list
data Command = Store FilePath B.ByteString | View FilePath | ViewRaw FilePath 

-- Settings for index assumption
--   if _assumeIndex is true, then we will assume that 
--      /path/to/file/${_indexAssumption} is the working index page
--   Index will be assumed if a path is requested without an explicit file name
--   If _assumeIndex is false, then we wont assume anything. 
--     note: server will return an error if just a path is requested
data IndexSettings = IndexSettings {
    _assumeIndex     :: Bool
  , _indexAssumption :: FilePath
} deriving (Show)

data Msg = Msg {
    _command :: String
  , _password :: Maybe String
  , _filepath :: FilePath
  , _filedata :: Maybe T.Text
}

-- Support for mime types
data CustomOut = PDF | OGG | MP4 | SVG | ICO | PNG | GIF | WEBM | HTML | JSON | JPG | JPEG | CSS | JS | TXT | Unsupported

-- Support for binary or utf-8 text files
data FileEncoding = UTF8 | Binary deriving (Eq, Ord)

data PreppedFile = PreppedFile {
    _getText   :: Maybe TL.Text
  , _getBinary :: Maybe BL.ByteString
} 

instance FromJSON Msg where
  parseJSON (Object v) = 
    Msg <$>
    (v .:  "command")  <*>
    (v .:? "password") <*>
    (v .:  "path")     <*>
    (v .:? "data") 

makeLenses ''State
makeLenses ''Page
