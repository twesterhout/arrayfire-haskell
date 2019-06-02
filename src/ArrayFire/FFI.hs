{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ArrayFire.FFI where

import ArrayFire.Exception
import ArrayFire.Types

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Array

import Control.Exception
import Control.Monad
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc
import System.IO.Unsafe

op3
  :: Array a
  -> Array a
  -> Array a
  -> (Ptr AFArray -> AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array a
op3 (Array fptr1) (Array fptr2) (Array fptr3) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
         withForeignPtr fptr3 $ \ptr3 -> do
           ptr <- 
             alloca $ \ptrInput -> do
               throwAFError =<< op ptrInput ptr1 ptr2 ptr3
               peek ptrInput
           fptr <- newForeignPtr af_release_array_finalizer ptr
           pure (Array fptr)

op2
  :: Array a
  -> Array a
  -> (Ptr AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array a
op2 (Array fptr1) (Array fptr2) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
        ptr <-
          alloca $ \ptrInput -> do
            throwAFError =<< op ptrInput ptr1 ptr2
            peek ptrInput
        fptr <- newForeignPtr af_release_array_finalizer ptr
        pure (Array fptr)

op1
  :: Array a
  -> (Ptr AFArray -> AFArray -> IO AFErr)
  -> Array a
op1 (Array fptr1) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      ptr <-
        alloca $ \ptrInput -> do
          throwAFError =<< op ptrInput ptr1
          peek ptrInput
      fptr <- newForeignPtr af_release_array_finalizer ptr
      pure (Array fptr)

afCall
  :: IO AFErr
  -> IO ()
afCall = (throwAFError =<<)

afCall1
  :: Storable a
  => (Ptr a -> IO AFErr)
  -> IO a
afCall1 op =
  alloca $ \ptrInput -> do
    throwAFError =<< op ptrInput
    peek ptrInput

infoFromArray
  :: Storable a
  => Array b
  -> (Ptr a -> AFArray -> IO AFErr)
  -> a
infoFromArray (Array fptr1) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput -> do
        throwAFError =<< op ptrInput ptr1
        peek ptrInput

infoFromArray2
  :: (Storable a, Storable b)
  => Array arr
  -> (Ptr a -> Ptr b -> AFArray -> IO AFErr)
  -> (a,b)
infoFromArray2 (Array fptr1) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput1 -> do
        alloca $ \ptrInput2 -> do
          throwAFError =<< op ptrInput1 ptrInput2 ptr1
          (,) <$> peek ptrInput1 <*> peek ptrInput2

infoFromArray3
  :: (Storable a, Storable b, Storable c)
  => Array arr
  -> (Ptr a -> Ptr b -> Ptr c -> AFArray -> IO AFErr)
  -> (a,b,c)
infoFromArray3 (Array fptr1) op =
  unsafePerformIO $
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput1 -> do
        alloca $ \ptrInput2 -> do
          alloca $ \ptrInput3 -> do
            throwAFError =<< op ptrInput1 ptrInput2 ptrInput3 ptr1
            (,,) <$> peek ptrInput1
                 <*> peek ptrInput2
                 <*> peek ptrInput3