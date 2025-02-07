{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Device
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- Information about ArrayFire API and devices
--
-- >>> info
--  ArrayFire v3.6.4 (OpenCL, 64-bit Mac OSX, build 1b8030c5)
-- [0] APPLE: AMD Radeon Pro 555X Compute Engine, 4096 MB
-- -1- APPLE: Intel(R) UHD Graphics 630, 1536 MB
--
--------------------------------------------------------------------------------
module ArrayFire.Device where

import Foreign.C.String
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import ArrayFire.Internal.Device
import ArrayFire.Internal.Types (AFType, Array)
import ArrayFire.FFI

-- | Retrieve info from ArrayFire API
--
-- @
-- ArrayFire v3.6.4 (OpenCL, 64-bit Mac OSX, build 1b8030c5)
-- [0] APPLE: AMD Radeon Pro 555X Compute Engine, 4096 MB
-- -1- APPLE: Intel(R) UHD Graphics 630, 1536 MB
-- @
info :: IO ()
info = afCall af_info

-- | Calls /af_init/ C function from ArrayFire API
--
-- >>> afInit
-- ()
afInit :: IO ()
afInit = afCall af_init

-- | Retrieves ArrayFire device information as 'String', same as 'info'.
--
-- >>> getInfoString
-- "ArrayFire v3.6.4 (OpenCL, 64-bit Mac OSX, build 1b8030c5)\n[0] APPLE: AMD Radeon Pro 555X Compute Engine, 4096 MB\n-1- APPLE: Intel(R) UHD Graphics 630, 1536 MB\n"
getInfoString :: IO String
getInfoString = peekCString =<< afCall1 (flip af_info_string 1)

-- af_err af_device_info(char* d_name, char* d_platform, char *d_toolkit, char* d_compute);

-- | Retrieves count of devices
--
-- >>> getDeviceCount
-- 2
getDeviceCount :: IO Int
getDeviceCount = fromIntegral <$> afCall1 af_get_device_count

-- af_err af_get_dbl_support(bool* available, const int device);
-- | Sets a device by 'Int'
--
-- >>> setDevice 0
-- ()
setDevice :: Int -> IO ()
setDevice (fromIntegral -> x) = afCall (af_set_device x)

-- | Retrieves device identifier
--
-- >>> getDevice
-- 0
getDevice :: IO Int
getDevice = fromIntegral <$> afCall1 af_get_device

-- af_err af_get_device_ptr(void **ptr, const af_array arr);
-- | Retrieves the raw device pointer
--
-- You should call 'unsafeUnlockDevicePtr' when you are done with the pointer to return control back
-- to ArrayFire.
--
-- __Note:__ this function may only be called after 'manualEval' to force the evaluation. See
-- <https://arrayfire.org/docs/interop_cuda.htm> for an example.
--
-- @since 0.6.1.0
unsafeGetDevicePtr
  :: AFType a
  => Array a
  -> IO (Ptr a)
unsafeGetDevicePtr arr =
  alloca $ \dataPtrPtr -> do
    inPlace arr (af_get_device_ptr dataPtrPtr)
    castPtr <$> peek dataPtrPtr

-- af_err af_lock_device_ptr(const af_array arr);
-- | Return control of an array back to ArrayFire.
--
-- @since 0.6.1.0
unsafeUnlockDevicePtr
  :: AFType a
  => Array a
  -> IO ()
unsafeUnlockDevicePtr = (`inPlace` af_unlock_device_ptr)

-- af_err af_sync(const int device);
-- af_err af_alloc_device(void **ptr, const dim_t bytes);
-- af_err af_free_device(void *ptr);
-- af_err af_alloc_pinned(void **ptr, const dim_t bytes);
-- af_err af_free_pinned(void *ptr);
-- af_err af_alloc_host(void **ptr, const dim_t bytes);
-- af_err af_free_host(void *ptr);
-- af_err af_device_array(af_array *arr, const void *data, const unsigned ndims, const dim_t * const dims, const af_dtype type);
-- af_err af_device_mem_info(size_t *alloc_bytes, size_t *alloc_buffers, size_t *lock_bytes, size_t *lock_buffers);
-- af_err af_print_mem_info(const char *msg, const int device_id);
-- af_err af_device_gc();
-- af_err af_set_mem_step_size(const size_t step_bytes);
-- af_err af_get_mem_step_size(size_t *step_bytes);
-- af_err af_lock_device_ptr(const af_array arr);
-- af_err af_unlock_device_ptr(const af_array arr);
-- af_err af_lock_array(const af_array arr);
-- af_err af_is_locked_array(bool *res, const af_array arr);
