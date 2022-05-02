module VirtualFields
  ( firstName
  , lastName
  , username
  , email
  , User
  ) where

import Control.Lens (Lens', makeLenses)
import Data.Text    (Text)
import GHC.Generics (Generic)

data User = User
  { _firstName :: Text
  , _lastName  :: Text
  , _email     :: Text
  } deriving (Eq, Generic, Show)
makeLenses ''User

-- We’ve decided we’re no longer going to have separate usernames and emails; now the email will be
-- used in place of a username. Your task is to delete the _username field and write a replacement
-- username lens which reads and writes from/to the _email field instead. The change should be
-- unnoticed by those importing the module.
username :: Lens' User Text
username = email
