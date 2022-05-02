{-# OPTIONS_GHC -Wno-orphans #-}
module LawsSpec (spec) where

import Control.Lens        (view)
import Data.List           (intercalate)
import Test.Hspec          (Spec, describe, it)
import Test.QuickCheck     (oneof)
import Test.Validity       (GenValid (..), Validity (..))
import Test.Validity.Utils (shouldFail)

import Laws
import LensProps

instance Validity Helper where
  validate = mempty

instance GenValid Helper where
  genValid = newHelper <$> genValid
  shrinkValid (Helper b n) = Helper b <$> shrinkValid n

instance Validity Helper2 where
  validate = mempty

instance GenValid Helper2 where
  genValid = newHelper2 <$> genValid
  shrinkValid (Helper2 x y) = Helper2 x <$> shrinkValid y

instance Validity Err where
  validate = mempty

instance GenValid Err where
  genValid = do
    msgOrCode <- genValid
    if msgOrCode
       then pure $ ReallyBadError "some error"
       else ExitCode <$> genValid
  shrinkValid _ = []

instance Validity Builder where
  validate = mempty

instance GenValid Builder where
  genValid = do
    _context <- genValid
    _build <- oneof [pure concat, pure (intercalate ",")]
    pure Builder{..}
  shrinkValid _ = []

spec :: Spec
spec = do
  describe "breaking the laws" $ do
    describe "breaksSecondLaw" $ do
      checkSetGet breaksSecondLaw
      it "breaks the second law" $
        shouldFail $ pGetSet breaksSecondLaw
      checkSetSet breaksSecondLaw
    describe "breaksThirdLaw" $ do
      checkSetGet breaksThirdLaw
      checkGetSet breaksThirdLaw
      it "breaks the third law" $
        shouldFail $ pSetSet breaksThirdLaw

  describe "msg lens" $ do
    it "breaks set-get" $
      shouldFail $ pSetGet msg
    checkGetSet msg
    checkSetSet msg

  describe "msg2 lens" $ do
    it "breaks get-set" $
      shouldFail $ pGetSet msg2
    checkSetGet msg2
    checkSetSet msg2

  describe "builderLens" $ do
    let builderComp builder1 builder2 = view builderLens builder1 == view builderLens builder2
    checkSetGet builderLens
    it "satisfies set-set" $
      genericPropGetSet builderComp builderLens
    it "satisfies get-set" $
      genericPropGetSet builderComp builderLens
