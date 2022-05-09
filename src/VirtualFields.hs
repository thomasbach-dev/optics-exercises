module VirtualFields
  ( firstName
  , lastName
  , fullName
  , username
  , email
  , User
  , mkUser
  ) where

import qualified Data.Text as T

import Control.Lens (Lens', lens, makeLenses, set, view)
import Data.Char    (isSpace)
import Data.Text    (Text)
import GHC.Generics (Generic)

data User = User
  { _firstName :: Text
  , _lastName  :: Text
  , _email     :: Text
  } deriving (Eq, Generic, Show)
makeLenses ''User

mkUser :: Text -> Text -> Text -> User
mkUser = User

-- We’ve decided we’re no longer going to have separate usernames and emails; now the email will be
-- used in place of a username. Your task is to delete the _username field and write a replacement
-- username lens which reads and writes from/to the _email field instead. The change should be
-- unnoticed by those importing the module.
username :: Lens' User Text
username = email

-- Write a lens for the user’s fullName. It should append the first and last names when “getting”.
-- When “setting” treat everything till the first space as the first name, and everything following
-- it as the last name.
fullName :: Lens' User Text
fullName = lens getter setter
  where
    getter user = let firstN = view firstName user
                      lastN = view lastName user
                  in if lastN == ""
                        then firstN
                        else firstN <> " " <> lastN
    setter user name = let (first, lastN) = T.break isSpace name
                       in (set firstName first . set lastName (T.stripStart lastN)) user
