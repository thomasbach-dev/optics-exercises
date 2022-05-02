module VirtualFields
  ( firstName
  , lastName
  , username
  , email
  , User
  ) where

import Control.Lens (makeLenses)
import Data.Text    (Text)
import GHC.Generics (Generic)

data User = User
  { _firstName :: Text
  , _lastName  :: Text
  , _username  :: Text
  , _email     :: Text
  } deriving (Eq, Generic, Show)
makeLenses ''User
