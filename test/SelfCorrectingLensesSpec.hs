module SelfCorrectingLensesSpec
  ( spec
  ) where

import Control.Lens (set)
import Test.Hspec   (Spec, describe, it, shouldBe)

import SelfCorrectingLenses

spec :: Spec
spec = do
  describe "exercise 1" $ do
    let example1 = ProducePrices 1 2
    describe "limePrice1" $ do
      it "does not set a negative value" $ do
        set limePrice1 (-1) example1 `shouldBe` ProducePrices 0 2
    describe "lemonPrice1" $ do
      it "does not set a negative value" $ do
        set lemonPrice1 (-1) example1 `shouldBe` ProducePrices 1 0
  describe "exercise 2" $ do
    let prices = ProducePrices 1.5 1.48
    it "should replicate expected behaviour 1" $ do
      set limePrice2 2 prices `shouldBe` ProducePrices 2.0 1.5
    it "should replicate expected behaviour 2" $ do
      set limePrice2 1.8 prices `shouldBe` ProducePrices 1.8 1.48
    it "should replicate expected behaviour 3" $ do
      set limePrice2 1.63 prices `shouldBe` ProducePrices 1.63 1.48
    it "should replicate expected behaviour 3" $ do
      set limePrice2 (-1) prices `shouldBe` ProducePrices 0 0.5
