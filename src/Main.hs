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

data Ray = Ray Vec3 Vec3 deriving Show
data LineSeg = LineSeg Vec3 Vec3 deriving Show

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
        parallel = if d0 <= 0 && d1 <= 0
                       then norm (a0 &- (if abs d0 < abs d1 then b0 else b1))
                       else norm ((_A &* d0) &+ a0 &- b0)
        t = b0 &- a0
        detA = det3 t _B cross
        detB = det3 t _A cross
        t0 = detA / denom
        t1 = detB / denom
        pA = a0 &+ (_A &* t0)
        pB = b0 &+ (_B &* t1)
        pA' = if t0 < 0 then a0 else pA
        pB' = if t1 < 0 then b0 else (if t1 > magB then b1 else pB)
        clamp a b x = if x < a then a else (if x > b then b else x)
        dot1 = _B &. (pA' &- b0)
        pB'' = if t0 < 0 then b0 &+ (_B &* (clamp 0 magB dot1)) else pB'
        dot2 = _A &. (pB' &- a0)
        pA'' = if t1 < 0 || t1 > magB then a0 &+ (_A &* (max 0 dot2)) else pA'
        nonparallel = norm (pA'' &- pB'')

rotateCCW90 :: Pixel a => Image a -> Image a
rotateCCW90 image@Image {imageWidth, imageHeight} = generateImage pixel imageHeight imageWidth
    where
        pixel x y = pixelAt image ((imageWidth - 1) - y) x

cube :: [LineSeg]
cube = [LineSeg (cubeVerts !! a) (cubeVerts !! b) | b <- [0..7], a <- [0..b], oneCompDiff (cubeVerts !! a) (cubeVerts !! b)]
    where
        comp n i = if n `mod` (i * 2) >= i then 0.5 else -0.5
        cubeVerts = [Vec3 (comp n 1) (comp n 2) (comp n 4) | n <- [0..7]]
        oneCompDiff (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = length (filter id $ zipWith (==) [x1, y1, z1] [x2, y2, z2]) == 2

rotate :: Float -> Float -> Float -> Vec3 -> Vec3
rotate rx ry rz v = (rotMatrixZ rz) *. (rotMatrixY ry) *. (rotMatrixX rx) *. v

rotateLineSeg :: Float -> Float -> Float -> LineSeg -> LineSeg
rotateLineSeg rx ry rz (LineSeg v1 v2) = LineSeg (rotate rx ry rz v1) (rotate rx ry rz v2)

render :: Int -> Int -> [LineSeg] -> Image PixelRGB8
render width height lineSegs = generateImage pixel width height
    where
        pixel x y = PixelRGB8 d d d
            where
                md = fromIntegral $ min width height
                zoom = 0.4
                r = Ray (Vec3 0 0 (-2))
                        (Vec3 (((fromIntegral x) - ((fromIntegral width) / 2)) / md / zoom)
                              (((fromIntegral y) - ((fromIntegral height) / 2)) / md / zoom)
                              0)
                d = floor $ (255 -) $ min ((minimum (map (lineRayDistance r) lineSegs)) * 10000) 255

width  = 600
height = 480

main = do
    args <- getArgs
    let filenameFormat = args !! 0

    let numFrames = 10 :: Int
    forM_ [0..(numFrames - 1)] $ \frameNo -> do
        putStrLn (printf "%d / %d..." (frameNo + 1) numFrames)
        let t = (fromIntegral frameNo) / (fromIntegral numFrames)
        let rx =  t        * 2 * pi
        let ry = (t + 0.3) * 2 * pi
        let rz = (t + 0.7) * 2 * pi
        let rotCube = map (rotateLineSeg rx ry rz) cube
        let image = render width height rotCube
        writeBitmap (printf filenameFormat frameNo) $ {-rotateCCW90-} image
