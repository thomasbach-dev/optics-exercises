{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Laws
  ( breaksSecondLaw
  , newHelper
  , Helper(..)
  , breaksThirdLaw
  , newHelper2
  , Helper2(..)
  , Err(..)
  , msg
  , msg2
  ) where

import Control.Lens (Lens', lens)

data Helper = Helper Bool Integer
            deriving (Eq, Show)

newHelper :: Integer -> Helper
newHelper = Helper False

-- | Implement a lens which breaks the second law (get-set).
breaksSecondLaw :: Lens' Helper Integer
breaksSecondLaw = lens getter setter
  where
    getter (Helper _ n) = n
    setter _ = Helper True

data Helper2 = Helper2 Integer Integer
             deriving (Eq, Show)

newHelper2 :: Integer -> Helper2
newHelper2 = Helper2 0

-- | Implement a lens which breaks the third law (set-set)..
breaksThirdLaw :: Lens' Helper2 Integer
breaksThirdLaw = lens getter setter
  where
    getter (Helper2 x y) = y - x
    setter h@(Helper2 x y) n = if x == 0 && y == n
                                   then h
                                   else Helper2 (x + 1) (n + x + 1)

data Err = ReallyBadError String
         | ExitCode Int
         deriving (Eq, Show)

msg :: Lens' Err String
msg = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    getMsg (ExitCode _)             = ""
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    setMsg (ExitCode n) _                = ExitCode n


-- | There’s a different way we could have written the msg lens such that it would PASS the set-get
-- law and the set-set law, but fail get-set.
msg2 :: Lens' Err String
msg2 = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    getMsg (ExitCode _)             = ""
    setMsg _ newMessage = ReallyBadError newMessage
