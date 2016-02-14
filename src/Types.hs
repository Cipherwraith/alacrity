module Types where
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

newState :: State
newState = State HM.empty

data Page = Page { 
    _absolutePath :: FilePath
  , _data :: FileData
  }

data State = State {
    _pages :: HM.HashMap FilePath Page
  } 

type FileData = T.Text



