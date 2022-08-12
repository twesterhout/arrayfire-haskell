{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}
module ArrayFire.IndexSpec where

import qualified ArrayFire         as A
import           Control.Exception
import           Data.Complex
import           Data.Int
import           Data.Proxy
import           Data.Word
import           Foreign.C.Types
import           Test.Hspec

spec :: Spec
spec =
  describe "Index spec" $ do
    it "Should index into an array" $ do
      let arr = A.vector @Int 10 [1..]
      A.index arr [A.Seq 0 4 1]
        `shouldBe`
           A.vector @Int 5 [1..]
      A.lookup arr (A.vector @Int 3 [2, 3, 5]) 0 `shouldBe` (A.vector @Int 3 [3, 4, 6])
      A.lookup arr (A.vector @Float 2 [2, 5]) 0 `shouldBe` (A.vector @Int 2 [3, 6])
      let arr2 = A.transpose (A.mkArray @Float [5, 4] [1..]) False
      A.lookup arr2 (A.vector @Int 2 [0, 2]) 1
        `shouldBe`
          A.matrix @Float (4, 2) [[1, 6, 11, 16],
                                  [3, 8, 13, 18]]
    it "Should assign to an array" $ do
      let lhs = A.vector @Int 5 [1..]
          rhs = A.vector @Int 2 [0, 0]
      A.assign lhs [A.Seq 1 2 1] rhs
        `shouldBe` A.vector 5 [1, 0, 0, 4, 5]
      let lhs2 = A.mkArray @Int [3, 4] [1..]
          rhs2 = A.constant [3, 2] 0
      A.assign lhs2 [A.Seq 0 (-1) 1, A.Seq 1 2 1] rhs2
        `shouldBe`
          A.matrix (3, 4) [[1, 2, 3],
                           [0, 0, 0],
                           [0, 0, 0],
                           [10, 11, 12]]
