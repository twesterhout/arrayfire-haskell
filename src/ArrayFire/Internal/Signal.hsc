module ArrayFire.Internal.Signal where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "signal.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_approx2"
    af_approx2 :: Ptr AFArray -> AFArray -> AFArray -> AFArray -> AFInterpType -> Float -> IO AFErr
foreign import ccall unsafe "af_approx1_uniform"
    af_approx1_uniform :: Ptr AFArray -> AFArray -> AFArray -> Int -> Double -> Double -> AFInterpType -> Float -> IO AFErr
foreign import ccall unsafe "af_approx2_uniform"
    af_approx2_uniform :: Ptr AFArray -> AFArray -> AFArray -> Int -> Double -> Double -> AFArray -> Int -> Double -> Double -> AFInterpType -> Float -> IO AFErr
foreign import ccall unsafe "af_fft"
    af_fft :: Ptr AFArray -> AFArray -> Double -> Word64 -> IO AFErr
foreign import ccall unsafe "af_fft_inplace"
    af_fft_inplace :: AFArray -> Double -> IO AFErr
foreign import ccall unsafe "af_fft2"
    af_fft2 :: Ptr AFArray -> AFArray -> Double -> Word64 -> Word64 -> IO AFErr
foreign import ccall unsafe "af_fft2_inplace"
    af_fft2_inplace :: AFArray -> Double -> IO AFErr
foreign import ccall unsafe "af_fft3"
    af_fft3 :: Ptr AFArray -> AFArray -> Double -> Word64 -> Word64 -> Word64 -> IO AFErr
foreign import ccall unsafe "af_fft3_inplace"
    af_fft3_inplace :: AFArray -> Double -> IO AFErr
foreign import ccall unsafe "af_ifft"
    af_ifft :: Ptr AFArray -> AFArray -> Double -> Word64 -> IO AFErr
foreign import ccall unsafe "af_ifft_inplace"
    af_ifft_inplace :: AFArray -> Double -> IO AFErr
foreign import ccall unsafe "af_ifft2"
    af_ifft2 :: Ptr AFArray -> AFArray -> Double -> Word64 -> Word64 -> IO AFErr
foreign import ccall unsafe "af_ifft2_inplace"
    af_ifft2_inplace :: AFArray -> Double -> IO AFErr
foreign import ccall unsafe "af_ifft3"
    af_ifft3 :: Ptr AFArray -> AFArray -> Double -> Word64 -> Word64 -> Word64 -> IO AFErr
foreign import ccall unsafe "af_ifft3_inplace"
    af_ifft3_inplace :: AFArray -> Double -> IO AFErr
foreign import ccall unsafe "af_fft_r2c"
    af_fft_r2c :: Ptr AFArray -> AFArray -> Double -> Word64 -> IO AFErr
foreign import ccall unsafe "af_fft2_r2c"
    af_fft2_r2c :: Ptr AFArray -> AFArray -> Double -> Word64 -> Word64 -> IO AFErr
foreign import ccall unsafe "af_fft3_r2c"
    af_fft3_r2c :: Ptr AFArray -> AFArray -> Double -> Word64 -> Word64 -> Word64 -> IO AFErr
foreign import ccall unsafe "af_fft_c2r"
    af_fft_c2r :: Ptr AFArray -> AFArray -> Double -> Bool -> IO AFErr
foreign import ccall unsafe "af_fft2_c2r"
    af_fft2_c2r :: Ptr AFArray -> AFArray -> Double -> Bool -> IO AFErr
foreign import ccall unsafe "af_fft3_c2r"
    af_fft3_c2r :: Ptr AFArray -> AFArray -> Double -> Bool -> IO AFErr
foreign import ccall unsafe "af_convolve1"
    af_convolve1 :: Ptr AFArray -> AFArray -> AFArray -> AFConvMode -> AFConvDomain -> IO AFErr
foreign import ccall unsafe "af_convolve2"
    af_convolve2 :: Ptr AFArray -> AFArray -> AFArray -> AFConvMode -> AFConvDomain -> IO AFErr
foreign import ccall unsafe "af_convolve3"
    af_convolve3 :: Ptr AFArray -> AFArray -> AFArray -> AFConvMode -> AFConvDomain -> IO AFErr
foreign import ccall unsafe "af_convolve2_sep"
    af_convolve2_sep :: Ptr AFArray -> AFArray -> AFArray -> AFArray -> AFConvMode -> IO AFErr
foreign import ccall unsafe "af_fft_convolve1"
    af_fft_convolve1 :: Ptr AFArray -> AFArray -> AFArray -> AFConvMode -> IO AFErr
foreign import ccall unsafe "af_fft_convolve2"
    af_fft_convolve2 :: Ptr AFArray -> AFArray -> AFArray -> AFConvMode -> IO AFErr
foreign import ccall unsafe "af_fft_convolve3"
    af_fft_convolve3 :: Ptr AFArray -> AFArray -> AFArray -> AFConvMode -> IO AFErr
foreign import ccall unsafe "af_fir"
    af_fir :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_iir"
    af_iir :: Ptr AFArray -> AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_medfilt"
    af_medfilt :: Ptr AFArray -> AFArray -> Word64 -> Word64 -> AFBorderType -> IO AFErr
foreign import ccall unsafe "af_medfilt1"
    af_medfilt1 :: Ptr AFArray -> AFArray -> Word64 -> AFBorderType -> IO AFErr
foreign import ccall unsafe "af_medfilt2"
    af_medfilt2 :: Ptr AFArray -> AFArray -> Word64 -> Word64 -> AFBorderType -> IO AFErr
foreign import ccall unsafe "af_set_fft_plan_cache_size"
    af_set_fft_plan_cache_size :: Word -> IO AFErr