-- vim: ts=4:sw=4:et

{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Data.List
import Text.Printf
import System.Environment

import Codec.Picture
import Data.Vect.Float
import Data.Vect.Float.Base

data Ray = Ray Vec3 Vec3
data LineSeg = LineSeg Vec3 Vec3

lineRayDistance :: Ray -> LineSeg -> Float
lineRayDistance (Ray a0 a1) (LineSeg b0 b1) = if denom < epsilon then parallel else nonparallel
    where
        epsilon = 1e-6
        a = a1 &- a0
        b = b1 &- b0
        magA = norm a
        magB = norm b
        _A = a &* (1 / magA)
        _B = b &* (1 / magB)
        cross = _A &^ _B
        denom = normsqr cross
        d0 = _A &. (b0 &- a0)
        d1 = _A &. (b1 &- a0)
        parallel = if d0 <= magA && d1 <= magA
                       then norm (a1 &- (if abs d0 < abs d1 then b0 else b1))
                       else norm ((_A &* d0) &+ a0 &- b0)
        t = b0 &- a0
        detA = det3 t _B cross
        detB = det3 t _A cross
        t0 = detA / denom
        t1 = detB / denom
        pA = if t0 > magA then a1 else a0 &+ (_A &* t0)
        dt = _B &. (pA &- b0)
        pB = if t0 > magA then b0 &+ (_B &* dt) else b0 &+ (_B &* t1)
        nonparallel = norm (pA &- pB)

rotateCCW90 :: Pixel a => Image a -> Image a
rotateCCW90 image@Image {imageWidth, imageHeight} = generateImage pixel imageHeight imageWidth
    where
        pixel x y = pixelAt image ((imageWidth - 1) - y) x

cubeImage :: Int -> Int -> Float -> Image PixelRGB8
cubeImage width height t = generateImage pixel width height
    where
        pixel x y = PixelRGB8 d d d
            where
                md = fromIntegral $ min width height
                r = Ray (Vec3 0 0 (-100))
                        (Vec3 (((fromIntegral x) - ((fromIntegral width) / 2)) / md)
                              (((fromIntegral y) - ((fromIntegral height) / 2)) / md)
                              0)
                comp n i = if n `mod` (i * 2) >= i then 1 else -1
                cubeVerts = [Vec3 (comp n 1) (comp n 2) (comp n 4) | n <- [0..7]]
                rx = cos ( t        * 2 * pi)
                ry = cos ((t + 0.3) * 2 * pi)
                rz = cos ((t + 0.7) * 2 * pi)
                rotCubeVerts = [(rotMatrixX rx) *. (rotMatrixY ry) *. (rotMatrixZ rz) *. v | v <- cubeVerts]
                cube = [LineSeg (cubeVerts !! n) (rotCubeVerts !! ((n + 2) `mod` (length rotCubeVerts))) | n <- [0..7]]
                d = floor $ min ((minimum (map (lineRayDistance r) cube)) * 2000) 255

width  = 600
height = 200

main = do
    args <- getArgs
    let filenameFormat = args !! 0

    let numFrames = 100 :: Int
    forM_ [0..(numFrames - 1)] $ \frameNo -> do
        let cube = cubeImage width height ((fromIntegral frameNo) / (fromIntegral numFrames))
        writeBitmap (printf filenameFormat frameNo) $ {-rotateCCW90-} cube
