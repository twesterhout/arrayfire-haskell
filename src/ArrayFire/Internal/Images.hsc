module ArrayFire.Internal.Images where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "images.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_load_image"
    af_load_image :: Ptr AFArray -> Ptr Char -> Bool -> IO AFErr
foreign import ccall unsafe "af_save_image"
    af_save_image :: Ptr Char -> AFArray -> IO AFErr
foreign import ccall unsafe "af_load_image_memory"
    af_load_image_memory :: Ptr AFArray -> Ptr () -> IO AFErr
foreign import ccall unsafe "af_save_image_memory"
    af_save_image_memory :: Ptr (Ptr ()) -> AFArray -> AFImageFormat -> IO AFErr
foreign import ccall unsafe "af_delete_image_memory"
    af_delete_image_memory :: Ptr () -> IO AFErr
foreign import ccall unsafe "af_load_image_native"
    af_load_image_native :: Ptr AFArray -> Ptr Char -> IO AFErr
foreign import ccall unsafe "af_save_image_native"
    af_save_image_native :: Ptr Char -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_image_io_available"
    af_is_image_io_available :: Ptr Bool -> IO AFErr
foreign import ccall unsafe "af_resize"
    af_resize :: Ptr AFArray -> AFArray -> Word64 -> Word64 -> AFInterpType -> IO AFErr
foreign import ccall unsafe "af_transform"
    af_transform :: Ptr AFArray -> AFArray -> AFArray -> Word64 -> Word64 -> AFInterpType -> Bool -> IO AFErr
foreign import ccall unsafe "af_transform_coordinates"
    af_transform_coordinates :: Ptr AFArray -> AFArray -> Float -> Float -> IO AFErr
foreign import ccall unsafe "af_rotate"
    af_rotate :: Ptr AFArray -> AFArray -> Float -> Bool -> AFInterpType -> IO AFErr
foreign import ccall unsafe "af_translate"
    af_translate :: Ptr AFArray -> AFArray -> Float -> Float -> Word64 -> Word64 -> AFInterpType -> IO AFErr
foreign import ccall unsafe "af_scale"
    af_scale :: Ptr AFArray -> AFArray -> Float -> Float -> Word64 -> Word64 -> AFInterpType -> IO AFErr
foreign import ccall unsafe "af_skew"
    af_skew :: Ptr AFArray -> AFArray -> Float -> Float -> Word64 -> Word64 -> AFInterpType -> Bool -> IO AFErr
foreign import ccall unsafe "af_histogram"
    af_histogram :: Ptr AFArray -> AFArray -> Word32 -> Double -> Double -> IO AFErr
foreign import ccall unsafe "af_dilate"
    af_dilate :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_dilate3"
    af_dilate3 :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_erode"
    af_erode :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_erode3"
    af_erode3 :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_bilateral"
    af_bilateral :: Ptr AFArray -> AFArray -> Float -> Float -> Bool -> IO AFErr
foreign import ccall unsafe "af_mean_shift"
    af_mean_shift :: Ptr AFArray -> AFArray -> Float -> Float -> Word32 -> Bool -> IO AFErr
foreign import ccall unsafe "af_minfilt"
    af_minfilt :: Ptr AFArray -> AFArray -> Word64 -> Word64 -> AFBorderType -> IO AFErr
foreign import ccall unsafe "af_maxfilt"
    af_maxfilt :: Ptr AFArray -> AFArray -> Word64 -> Word64 -> AFBorderType -> IO AFErr
foreign import ccall unsafe "af_regions"
    af_regions :: Ptr AFArray -> AFArray -> AFConnectivity -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_sobel_operator"
    af_sobel_operator :: Ptr AFArray -> Ptr AFArray -> AFArray -> Word32 -> IO AFErr
foreign import ccall unsafe "af_rgb2gray"
    af_rgb2gray :: Ptr AFArray -> AFArray -> Float -> Float -> Float -> IO AFErr
foreign import ccall unsafe "af_gray2rgb"
    af_gray2rgb :: Ptr AFArray -> AFArray -> Float -> Float -> Float -> IO AFErr
foreign import ccall unsafe "af_hist_equal"
    af_hist_equal :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_gaussian_kernel"
    af_gaussian_kernel :: Ptr AFArray -> Int -> Int -> Double -> Double -> IO AFErr
foreign import ccall unsafe "af_hsv2rgb"
    af_hsv2rgb :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_rgb2hsv"
    af_rgb2hsv :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_color_space"
    af_color_space :: Ptr AFArray -> AFArray -> AFCspaceT -> AFCspaceT -> IO AFErr
foreign import ccall unsafe "af_unwrap"
    af_unwrap :: Ptr AFArray -> AFArray -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Bool -> IO AFErr
foreign import ccall unsafe "af_wrap"
    af_wrap :: Ptr AFArray -> AFArray -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Bool -> IO AFErr
foreign import ccall unsafe "af_sat"
    af_sat :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_ycbcr2rgb"
    af_ycbcr2rgb :: Ptr AFArray -> AFArray -> AFYccStd -> IO AFErr
foreign import ccall unsafe "af_rgb2ycbcr"
    af_rgb2ycbcr :: Ptr AFArray -> AFArray -> AFYccStd -> IO AFErr
foreign import ccall unsafe "af_moments"
    af_moments :: Ptr AFArray -> AFArray -> AFMomentType -> IO AFErr
foreign import ccall unsafe "af_moments_all"
    af_moments_all :: Ptr Double -> AFArray -> AFMomentType -> IO AFErr
foreign import ccall unsafe "af_canny"
    af_canny :: Ptr AFArray -> AFArray -> AFCannyThreshold -> Float -> Float -> Word32 -> Bool -> IO AFErr
foreign import ccall unsafe "af_anisotropic_diffusion"
    af_anisotropic_diffusion :: Ptr AFArray -> AFArray -> Float -> Float -> Word32 -> AFFluxFunction -> AFDiffusionEq -> IO AFErr
foreign import ccall unsafe "af_iterative_deconv"
    af_iterative_deconv :: Ptr AFArray -> AFArray -> AFArray -> Word32 -> Float -> AFIterativeDeconvAlgo -> IO AFErr
foreign import ccall unsafe "af_inverse_deconv"
    af_inverse_deconv :: Ptr AFArray -> AFArray -> AFArray -> Float -> AFInverseDeconvAlgo -> IO AFErr