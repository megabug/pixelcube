-- vim: sw=4:et

{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Text.Printf
import System.Environment

import Codec.Picture

rotateCCW90 :: Pixel a => Image a -> Image a
rotateCCW90 image@Image {imageWidth, imageHeight} = generateImage pixel imageHeight imageWidth
    where
        pixel x y = pixelAt image ((imageWidth - 1) - y) x

cubeImage :: Int -> Int -> Int -> Image PixelRGB8
cubeImage frameNo = generateImage pixel
    where
        pixel x y = PixelRGB8 (fromIntegral (x `mod` 256)) (fromIntegral (y `mod` 256)) 0

width  = 600
height = 200

main = do
    args <- getArgs
    let filenameFormat = args !! 0

    forM_ [0..7] $ \frameNo -> do
        let cube = cubeImage frameNo width height
        writeBitmap (printf filenameFormat frameNo) $ rotateCCW90 cube
