-- vim: ts=4:sw=4:et

{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Data.List
import Data.Function
import Text.Printf
import System.Environment

import Codec.Picture
import Data.Vect.Float
import Data.Vect.Float.Base

data Ray = Ray Vec3 Vec3 deriving Show
data LineSeg = LineSeg Vec3 Vec3 Vec3 deriving Show

closestLineRayPts :: Ray -> LineSeg -> (Vec3, Vec3)
closestLineRayPts (Ray a0 a1) (LineSeg b0 b1 _) = if denom < epsilon then parallel else nonparallel
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
        parallel = (a0, if d0 <= 0 && d1 <= 0
                        then (if abs d0 < abs d1 then b0 else b1)
                        else (b0 &+ (_A &* d0)))
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
        nonparallel = (pA'', pB'')

lineRayDistance :: Ray -> LineSeg -> Float
lineRayDistance a b = norm (a' &- b')
    where
        (a', b') = closestLineRayPts a b

rotateCCW90 :: Pixel a => Image a -> Image a
rotateCCW90 image@Image {imageWidth, imageHeight} = generateImage pixel imageHeight imageWidth
    where
        pixel x y = pixelAt image ((imageWidth - 1) - y) x

cube :: Float -> [LineSeg]
cube rad = (zipWith color [(cubeVerts !! a, cubeVerts !! b) | b <- [0..7], a <- [0..b], oneCompDiff (cubeVerts !! a) (cubeVerts !! b)] [0..])-- ++ [LineSeg v (v &+ (Vec3 epsilon 0 0)) (Vec3 1000 1000 1000) | v <- cubeVerts]
    where
        epsilon = 1e-6
        comp n i = n `mod` (i * 2) >= i
        vcomp n i = if comp n i then rad else -rad
        cubeVerts = [Vec3 (vcomp n 1) (vcomp n 2) (vcomp n 4) | n <- [0..7]]
        ccomp n i = if comp n i then 255 else 127
        color (a, b) n = let n' = n `mod` 6 + 1 in LineSeg a b (Vec3 255 255 255)--0 (ccomp n' 1) (ccomp n' 2))
        oneCompDiff (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = length (filter id $ zipWith (==) [x1, y1, z1] [x2, y2, z2]) == 2

rotate :: Float -> Float -> Float -> Vec3 -> Vec3
rotate rx ry rz v = (rotMatrixZ rz) *. (rotMatrixY ry) *. (rotMatrixX rx) *. v

rotateLineSeg :: Float -> Float -> Float -> LineSeg -> LineSeg
rotateLineSeg rx ry rz (LineSeg v1 v2 c) = LineSeg (rotate rx ry rz v1) (rotate rx ry rz v2) c

orthoCamera :: Float -> Float -> Float -> Float -> Float -> Ray
orthoCamera zoom width height x y = Ray (Vec3 0 0 (-2))
                                        (Vec3 ((x - (width  / 2)) / md / zoom)
                                              ((y - (height / 2)) / md / zoom)
                                              0)
    where
        md = min width height

circularCamera :: Float -> Float -> Float -> Float -> Float -> Ray
circularCamera rad width height x y = Ray (Vec3 0 0 (-2))
                                          (Vec3 (cos ((-x / width * 2 + 0.5) * pi) * rad)
                                                ((y / height - 0.5) * 2 * rad)
                                                (sin ((-x / width * 2 + 0.5) * pi) * rad))

onComp :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
onComp f (Vec3 a b c) (Vec3 x y z) = Vec3 (f a x) (f b y) (f c z)

render :: Int -> Int -> (Float -> Float -> Float -> Float -> Ray) -> Int -> [LineSeg] -> (Int -> Int -> PixelRGB8)
render width height camera fade lineSegs x y = conv v
    where
        t = camera (fromIntegral width) (fromIntegral height) (fromIntegral x) (fromIntegral y)
        v = onComp min (Vec3 255 255 255) $ foldr1 (&+) (map lc lineSegs)
        lc l@(LineSeg _ _ c) = if signum cpbz <= signum tz then c &* (1 / (((lineRayDistance t l) + 1) ^ fade)) else Vec3 0 0 0
            where
                Ray _ (Vec3 _ _ tz) = t
                (_, (Vec3 _ _ cpbz)) = closestLineRayPts t l

conv (Vec3 r g b) = PixelRGB8 (floor r) (floor g) (floor b)

border :: Int -> Int -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> (Int -> Int -> PixelRGB8) -> (Int -> Int -> PixelRGB8)
border width height l r u d p x y | x == 0          = conv l
                                  | x == width - 1  = conv r
                                  | y == 0          = conv u
                                  | y == height - 1 = conv d
                                  | otherwise       = p x y

ticks :: Int -> Int -> Vec3 -> (Int -> Int -> PixelRGB8) -> (Int -> Int -> PixelRGB8)
ticks width height c p x y = if x `elem` [floor $ (fromIntegral width) / 4 * n | n <- [0..3]] && y >= height - 10 then conv c else p x y

width  = floor $ (fromIntegral height) * pi
height = 200

main = do
    args <- getArgs
    let filenameFormat = args !! 0

    let numFrames = 17 :: Int
    forM_ [0..(numFrames - 1)] $ \frameNo -> do
        putStrLn (printf "%d / %d..." (frameNo + 1) numFrames)
        let t = (fromIntegral frameNo) / (fromIntegral numFrames)
        let rx = 0--(t + 0.7) * ( 4) * pi
        let ry = t * 2 * pi-- t        * (-2) * pi
        let rz = 0--(t + 0.2) * ( 2) * pi
        let rotCube = map (rotateLineSeg rx ry rz) (cube 0.33)
        let image = generateImage (ticks width height (Vec3 255 0 255) (border width height (Vec3 255 0 0) (Vec3 0 255 0) (Vec3 255 255 0) (Vec3 0 160 255) (render width height (circularCamera (sqrt (2 * 0.5 ^ 2) + 0.1)) 60 rotCube))) width height
        writeBitmap (printf filenameFormat frameNo) $ rotateCCW90 image
