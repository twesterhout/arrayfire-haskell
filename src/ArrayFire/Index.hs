--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Index
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- Functions for indexing into an 'Array'
--
--------------------------------------------------------------------------------
module ArrayFire.Index where

import ArrayFire.Internal.Index
import ArrayFire.Internal.Types
import ArrayFire.FFI
import ArrayFire.Exception

import Foreign

import System.IO.Unsafe
import Control.Exception

-- | Index into an 'Array' by 'Seq'
index
  :: Array a
  -- ^ 'Array' argument
  -> [Seq]
  -- ^ 'Seq' to use for indexing
  -> Array a
index (Array fptr) seqs =
  unsafePerformIO . mask_ . withForeignPtr fptr $ \ptr -> do
    alloca $ \aptr ->
      withSeqs seqs $ \n sptr -> do
        throwAFError =<< af_index aptr ptr n sptr
        Array <$> do
          newForeignPtr af_release_array_finalizer
            =<< peek aptr

-- | Lookup an Array by keys along a specified dimension
lookup :: Array a -> Array b -> Int -> Array a
lookup arr indices n =
  op2 indices arr $ \p indices' arr' ->
    af_lookup p arr' indices' (fromIntegral n)

-- | @assign lhs slice rhs@ is the equivalent of Python's @lhs[slice] = rhs@, i.e. we assign 'rhs'
-- to 'lhs' at indices specified by 'slice'.
assign
  :: AFType a
  => Array a
  -- ^ Left-hand side
  -> [Seq]
  -- ^ Slice of the left-hand side
  -> Array a
  -- ^ Right-hand side of the assignment
  -> Array a
  -- ^ Updated array
assign lhs slice rhs =
  op2 lhs rhs $ \outPtr lhsPtr rhsPtr ->
    withSeqs slice $ \ndims slicePtr ->
      af_assign_seq outPtr lhsPtr ndims slicePtr rhsPtr

-- af_err af_index_gen(  af_array *out, const af_array in, const dim_t ndims, const af_index_t* indices);
-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
-- indexGen :: Array a -> Int -> [Index a] -> Array a -> Array a
-- indexGen = error "Not implemneted"

-- af_err af_assingn_gen( af_array *out, const af_array lhs, const dim_t ndims, const af_index_t* indices, const af_array rhs);
-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
-- assignGen :: Array a -> Int -> [Index a] -> Array a -> Array a
-- assignGen = error "Not implemneted"

-- af_err af_create_indexers(af_index_t** indexers);
-- af_err af_set_array_indexer(af_index_t* indexer, const af_array idx, const dim_t dim);
-- af_err af_set_seq_indexer(af_index_t* indexer, const af_seq* idx, const dim_t dim, const bool is_batch);
-- af_err af_set_seq_param_indexer(af_index_t* indexer, const double begin, const double end, const double step, const dim_t dim, const bool is_batch);
-- af_err af_release_indexers(af_index_t* indexers);
