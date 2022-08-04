{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.ImageSpec where

import Control.Exception
import Data.Complex
import Data.Word
import Foreign.C.Types
import GHC.Int
import Test.Hspec

import ArrayFire

spec :: Spec
spec =
  describe "Image tests" $ do
    it "Should test if Image I/O is available" $ do
      -- We want the function to run to make sure there are no segfaults or related issues, but
      -- we cannot assume that it should return True because ArrayFire could have been compiled
      -- without graphics support
      r <- isImageIOAvailable
      r `shouldBe` r
