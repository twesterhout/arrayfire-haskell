{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import ArrayFire (AFType, Array)
import qualified ArrayFire
import Control.Exception (throwIO)
import Control.Monad (unless)
import GHC.Exts (IsList (..))
import GHC.Stack
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec
import Test.Hspec.QuickCheck

compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> a -> a -> Expectation
compareWith comparator result expected =
  unless (comparator result expected) $ do
    throwIO (HUnitFailure location $ ExpectedButGot Nothing expectedMsg actualMsg)
 where
  expectedMsg = show expected
  actualMsg = show result
  location = case reverse (toList callStack) of
    (_, loc) : _ -> Just loc
    [] -> Nothing

class (Num a) => HasEpsilon a where
  eps :: a

instance HasEpsilon Float where
  eps = 1.1920929e-7

instance HasEpsilon Double where
  eps = 2.220446049250313e-16

approxWith :: (Ord a, Num a) => a -> a -> a -> a -> Bool
approxWith rtol atol a b = abs (a - b) <= Prelude.max atol (rtol * Prelude.max (abs a) (abs b))

approx :: (Ord a, HasEpsilon a) => a -> a -> Bool
approx a b = approxWith (2 * eps * Prelude.max (abs a) (abs b)) (4 * eps) a b

shouldBeApprox :: (Ord a, HasEpsilon a, Show a) => a -> a -> Expectation
shouldBeApprox = compareWith approx

evalf :: (AFType a) => Array a -> a
evalf = ArrayFire.getScalar
