module VirtualFieldsSpec
  ( spec
  ) where

import Control.Lens          (set, view)
import Data.GenValidity.Text ()
import Data.Validity.Text    ()
import Test.Hspec            (Spec, describe, it, shouldBe)
import Test.Validity         (GenValid, Validity)

import LensProps
import VirtualFields

instance Validity User
instance GenValid User

spec :: Spec
spec = do
  describe "firstName lens" $
    checkLensLaws firstName
  describe "lastName lens" $
    checkLensLaws lastName
  describe "username lens" $
    checkLensLaws username
  describe "email lens" $
    checkLensLaws email
  describe "fullName" $ do
    it "passes the example" $ do
      let user = mkUser "John" "Cena" "invisible@example.com"
      view fullName user `shouldBe` "John Cena"
      set fullName "Docter of Thugonamics" user
        `shouldBe` mkUser "Docter" "of Thugonamics" "invisible@example.com"
    checkLensLaws fullName
