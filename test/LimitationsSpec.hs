module LimitationsSpec (spec) where

import Control.Lens           (set, view)
import Test.Hspec             (Spec, describe, it, shouldBe)
import Test.Validity.Property (forAllValid)

import LensLaws
import Limitations

spec :: Spec
spec = do
  describe "conditional" $ do
    it "should get the right field" $ forAllValid $ \t@(x, y :: Int, z :: Int) ->
      let expected = if x then y else z
      in view conditional t `shouldBe` expected
    it "should set the second field when True" $ forAllValid $ \(x, y, z) ->
      set conditional x (True, y, z) `shouldBe` (True, x, z :: Int)
    it "should set the third field when False" $ forAllValid $ \(x, y, z) ->
      set conditional x (False, y, z) `shouldBe` (False, y, x :: Int)

    checkLensLaws @Int conditional
