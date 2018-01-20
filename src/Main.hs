-- vim: ts=4:sw=4:et

{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Data.Function
import Data.List
import System.Environment
import Text.Printf

import Codec.Picture
import Data.Vect.Float
import Data.Vect.Float.Base

data Ray = Ray Vec3 Vec3 deriving Show
data LineSeg = LineSeg Vec3 Vec3 Vec3 deriving Show

epsilon :: Float
epsilon = 1e-6

clamp :: Ord a => a -> a -> a -> a
clamp a b = max a . max b

intersectRay :: Ray -> LineSeg -> (Vec3, Vec3)
intersectRay (Ray a0 a1) (LineSeg b0 b1 _) | denom < epsilon = parallel
                                           | otherwise       = nonparallel
  where
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

    bp | abs d0 < abs d1 = b0
       | otherwise       = b1

    p | d0 <= 0 && d1 <= 0 = bp
      | otherwise          = b0 &+ (_A &* d0)

    parallel = (a0, p)

    t = b0 &- a0

    detA = det3 t _B cross
    detB = det3 t _A cross

    t0 = detA / denom
    t1 = detB / denom

    pA = a0 &+ (_A &* t0)
    pB = b0 &+ (_B &* t1)

    pA' | t0 < 0    = a0
        | otherwise = pA
    pB' | t1 < 0    = b0
        | t1 > magB = b1
        | otherwise = pB

    dot1 = _B &. (pA' &- b0)
    dot2 = _A &. (pB' &- a0)

    pA'' | t1 < 0 || t1 > magB = a0 &+ (_A &* (max 0 dot2))
         | otherwise           = pA'
    pB'' | t0 < 0              = b0 &+ (_B &* (clamp 0 magB dot1))
         | otherwise           = pB'

    nonparallel = (pA'', pB'')

rayDist :: Ray -> LineSeg -> Float
rayDist a b = norm (a' &- b')
  where
    (a', b') = intersectRay a b

rotateCCW90 :: Pixel a => Image a -> Image a
rotateCCW90 image@(Image {imageWidth, imageHeight}) =
    generateImage pixel imageHeight imageWidth
      where
        pixel x y = pixelAt image ((imageWidth - 1) - y) x

cube :: Float -> Vec3 -> Float -> [LineSeg]
cube rad color glow = cubeLines ++ cubePts
  where
    v n i | n `mod` (i * 2) >= i = rad
          | otherwise            = -rad
    cubeVerts = [Vec3 (v n 1) (v n 2) (v n 4) | n <- [0..7]]

    oneCompDiff (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
        length (filter id $ zipWith (==) [x1, y1, z1] [x2, y2, z2]) == 2

    cubeLines =
        [
            LineSeg (cubeVerts !! a) (cubeVerts !! b) color
            | b <- [0..7], a <- [0..b],
            oneCompDiff (cubeVerts !! a) (cubeVerts !! b)
        ]

    cubePts =
        [
            LineSeg v (v &+ (Vec3 epsilon 0 0)) (color &* glow)
            | v <- cubeVerts
        ]

rotate :: Float -> Float -> Float -> Vec3 -> Vec3
rotate rx ry rz v = (rotMatrixZ rz) *. (rotMatrixY ry) *. (rotMatrixX rx) *. v

rotateLineSeg :: Float -> Float -> Float -> LineSeg -> LineSeg
rotateLineSeg rx ry rz (LineSeg v1 v2 c) =
    LineSeg (rotate rx ry rz v1) (rotate rx ry rz v2) c

orthoCamera :: Float -> Float -> Float -> Float -> Float -> Ray
orthoCamera zoom width height x y = Ray (Vec3 0 0 (-2))
                                        (Vec3 ((x - (width  / 2)) / md / zoom)
                                              ((y - (height / 2)) / md / zoom)
                                              0)
  where
    md = min width height

circularCamera :: Float -> Float -> Float -> Float -> Float -> Float -> Ray
circularCamera rad backDist width height x y =
    Ray (Vec3 0 0 (-backDist))
        (Vec3 (cos (-x / width * 2 * pi) * rad)
              ((y / height - 0.5) * 2 * rad)
              (sin (-x / width * 2 * pi) * rad))

onComp :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
onComp f (Vec3 a b c) (Vec3 x y z) = Vec3 (f a x) (f b y) (f c z)

render :: Int -> Int -> (Float -> Float -> Float -> Float -> Ray) -> Int ->
          [LineSeg] -> (Int -> Int -> PixelRGB8)
render width height camera fade lineSegs x y = conv v
  where
    t = camera (fromIntegral width) (fromIntegral height)
               (fromIntegral x) (fromIntegral y)
    v = onComp min (Vec3 255 255 255) $ foldr1 (&+) (map lc lineSegs)
    lc l@(LineSeg _ _ c) | signum cpbz <= signum tz =
                               c &* (1 / (((rayDist t l) + 1) ^ fade))
                         | otherwise                = Vec3 0 0 0
      where
        Ray _ (Vec3 _ _ tz) = t
        (_, (Vec3 _ _ cpbz)) = intersectRay t l

conv (Vec3 r g b) = PixelRGB8 (floor r) (floor g) (floor b)

border :: Int -> Int -> Vec3 -> Vec3 -> Vec3 -> Vec3 ->
          (Int -> Int -> PixelRGB8) -> (Int -> Int -> PixelRGB8)
border width height l r u d p x y | x == 0          = conv l
                                  | x == width - 1  = conv r
                                  | y == 0          = conv u
                                  | y == height - 1 = conv d
                                  | otherwise       = p x y

ticks :: Int -> Int -> Vec3 -> (Int -> Int -> PixelRGB8) ->
         (Int -> Int -> PixelRGB8)
ticks width height c p x y | x `elem` xs && y >= height - 20 = conv c
                           | otherwise                       = p x y
  where
    xs = [floor $ (fromIntegral width) / 4 * n | n <- [1..3]]

width  = floor $ (fromIntegral height) * pi
height = 200

main = do
    args <- getArgs
    let filenameFormat = args !! 0

    let numFrames = 53
    forM_ [0..((numFrames :: Int) - 1)] $ \frameNo -> do
        let t = (fromIntegral frameNo) / (fromIntegral numFrames)

        putStrLn (printf "%d / %d..." (frameNo + 1) numFrames)

        let rx = (t + 0.7) * ( 4) * pi
        let ry =  t        * (-2) * pi
        let rz = (t + 0.2) * ( 2) * pi
        let rotCube = map (rotateLineSeg rx ry rz)
                          (cube 0.35 (Vec3 255 255 255) 3)

        let pixel = ticks width height (Vec3 255 0 255) $
                    border width height (Vec3 255 0 0) (Vec3 0 255 0)
                           (Vec3 255 255 0) (Vec3 0 160 255) $
                    render width height
                           (circularCamera (sqrt (2 * 0.5 ^ 2)) 15) 60
                           rotCube
        let image = generateImage pixel width height

        writeBitmap (printf filenameFormat frameNo) $ rotateCCW90 image
