module VirtualFieldsSpec
  ( spec
  ) where

import Data.GenValidity.Text ()
import Data.Validity.Text    ()
import Test.Hspec            (Spec, describe)
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
