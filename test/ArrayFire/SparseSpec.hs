{-# LANGUAGE TypeApplications #-}

module ArrayFire.SparseSpec where

import qualified ArrayFire as A
import Data.Complex
import Data.Int
import Data.Proxy
import Data.Word
import Foreign.C.Types
import Test.Hspec

spec :: Spec
spec =
  describe "Sparse spec" $ do
    it "Should create a sparse array" $ do
      (1 + 1) `shouldBe` 2
      let mat =
            flip A.createSparseArrayFromDense A.CSR $
              A.transpose (A.mkArray @Float [3, 3] [1, 0, 0, 2, 0, 3, 4, 5, 6]) False
          (values, rows, cols, storage) = A.sparseGetInfo mat
      storage `shouldBe` A.CSR
      values `shouldBe` A.mkArray [6] [1 .. 6]
      cols `shouldBe` A.mkArray [6] [0, 0, 2, 0, 1, 2]
      rows `shouldBe` A.mkArray [4] [0, 1, 3, 6]

-- `shouldBe` A.vector @Double 10 [0 ..]
