module Limitations where

import           Control.Lens (Lens', lens)

-- | Can you write a `Lens'` which focuses the second element of a
-- tuple if the provided `Bool` is `True` and the third element if
-- it’s `False`?
conditional :: Lens' (Bool, a, a) a
conditional = lens getCond setCond
  where
    getCond (True, x, _)  = x
    getCond (False, _, y) = y
    setCond (True, _, y) x  = (True, x, y)
    setCond (False, x, _) y = (False, x, y)
