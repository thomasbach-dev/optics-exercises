module LensProps
  ( checkLensLaws
  , checkSetGet
  , pSetGet
  , genericPropSetGet
  , checkGetSet
  , pGetSet
  , genericPropGetSet
  , checkSetSet
  , pSetSet
  , genericPropSetSet
  ) where

import Control.Lens           (Lens', set, view)
import Test.Hspec             (HasCallStack, SpecWith, describe, it)
import Test.QuickCheck        (Property)
import Test.Validity.Property (GenValid, forAllValid)

checkLensLaws :: (HasCallStack, Eq a, GenValid a, Show a, Eq s, GenValid s, Show s) => Lens' s a -> SpecWith ()
checkLensLaws aLens =
  describe "satisfies the lens laws" $ do
    checkSetGet aLens
    checkGetSet aLens
    checkSetSet aLens

checkSetGet :: (HasCallStack, Eq a, GenValid a, Show a, GenValid s, Show s) => Lens' s a -> SpecWith ()
checkSetGet aLens = it "satisfies the set-get law" $ pSetGet aLens

-- | If you set using a lens, then view through it, you should get back what you just set!
pSetGet :: (Eq a, GenValid a, Show a, GenValid s, Show s) => Lens' s a -> Property
pSetGet = genericPropSetGet (==)

genericPropSetGet :: (GenValid a, Show a, GenValid s, Show s)
                  => (a -> a -> Bool) -> Lens' s a -> Property
genericPropSetGet comp aLens = forAllValid $ \(newValue, structure) ->
  comp (view aLens (set aLens newValue structure)) newValue

checkGetSet :: (HasCallStack, Eq s, GenValid s, Show s) => Lens' s a -> SpecWith ()
checkGetSet aLens = it "satisfies the get-set law" $ pGetSet aLens

-- | If we set the focus to what that focus currently contains, then nothing should change!
pGetSet :: (Eq s, GenValid s, Show s) => Lens' s a -> Property
pGetSet = genericPropGetSet (==)

genericPropGetSet :: (GenValid s, Show s) => (s -> s -> Bool) -> Lens' s a -> Property
genericPropGetSet comp aLens = forAllValid $ \structure ->
  comp (set aLens (view aLens structure) structure) structure

checkSetSet :: (HasCallStack, GenValid a, Show a, Eq s, GenValid s, Show s) => Lens' s a -> SpecWith ()
checkSetSet aLens = it "satisfies the set-set law" $ pSetSet aLens

-- | If we set the focus twice, only the last set should have any visible effect.
pSetSet :: (GenValid a, Show a, Eq s, GenValid s, Show s) => Lens' s a -> Property
pSetSet aLens = forAllValid $ \(newValue, structure) ->
  set aLens newValue (set aLens newValue structure) == set aLens newValue structure

genericPropSetSet :: (GenValid a, Show a, GenValid s, Show s)
                  => (s -> s -> Bool) -> Lens' s a -> Property
genericPropSetSet comp aLens = forAllValid $ \(newValue, structure) ->
  comp (set aLens newValue (set aLens newValue structure)) (set aLens newValue structure)
