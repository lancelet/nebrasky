{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
module Nebrasky.Sampler where

import           Codec.Picture               (DynamicImage (ImageRGB8), Image,
                                              PixelRGB8 (PixelRGB8),
                                              generateImage, savePngImage)
import           Data.Fixed                  (mod')
import           Data.Monoid                 (mconcat)
import           Diagrams.Backend.Rasterific (B, renderRasterific)
import           Diagrams.Prelude            (Diagram, ( # ))
import qualified Diagrams.Prelude            as D
import           Linear.V2                   (V2 (V2))

saveTestImageNoAA :: IO ()
saveTestImageNoAA = do
    let
        width  = 256
        height = 256
        imageFn (V2 x y) = PixelRGB8 r r r
          where
            r = round (255.0 * testImage 2.0e-5 0.2 x y)
        image = rasteriseImageNoAA width height imageFn
    savePngImage "testImage-NoAA.png" (ImageRGB8 image)

rasteriseImageNoAA
    :: Int                       -- ^ width of raster (number of pixels)
    -> Int                       -- ^ height of raster (number of pixels)
    -> (V2 Float -> PixelRGB8)   -- ^ image sampling function
    -> Image PixelRGB8           -- ^ created image
rasteriseImageNoAA width height image = generateImage image' width height
  where
    image' :: Int -> Int -> PixelRGB8
    image' x y = image (V2 x' y')
      where
        x' = (fromIntegral x + 0.5) / fromIntegral width
        y' = (fromIntegral y + 0.5) / fromIntegral width


-- | Test image for antialiasing.
--
-- Taken from a Python version here:
-- https://gist.github.com/Reedbeta/893b63390160e33ddb3c#file-antialias-test-py-L36-L44
testImage
    :: Float -- ^ minimum period; 2.0e-5 is a good value
    -> Float -- ^ maximum period; 0.2 is a good value
    -> Float -- ^ x value; ideally in the range [0,1]
    -> Float -- ^ y value; ideally in the range [0,1]
    -> Float -- ^ computed image value
testImage minPeriod maxPeriod x y = round' ((x / period y) `mod'` 1.0)
  where
    period :: Float -> Float
    period q = minPeriod + (maxPeriod - minPeriod) * (q ^^ 2)

    round' :: Float -> Float
    round' = fromInteger . round

--- Diagrams

renderJitterCells :: IO ()
renderJitterCells = renderRasterific "jittercells.png" size (jitterCells 5 4)
  where
    size = D.dims (V2 500 500)

jitterCells
    :: Int
    -> Int
    -> Diagram B
jitterCells nx ny =
    let
        vLine :: Double -> Diagram B
        vLine x = D.fromVertices $ fmap D.p2 [ (x, 0.0), (x, 1.0) ]

        hLine :: Double -> Diagram B
        hLine y = D.fromVertices $ fmap D.p2 [ (0.0, y), (1.0, y) ]

    in
        mconcat
        [ mconcat $ fmap vLine $ linSpace 0.0 1.0 nx
        , mconcat $ fmap hLine $ linSpace 0.0 1.0 ny
        , (mconcat $ fmap vLine $ linSpace 0.0 1.0 (nx * nx)) # D.lc D.lightgray
        , (mconcat $ fmap hLine $ linSpace 0.0 1.0 (ny * ny)) # D.lc D.lightgray
        ]

linSpace
    :: Double
    -> Double
    -> Int
    -> [Double]
linSpace lmin lmax n = fmap scale [0 .. n]
  where
    scale i = (fromIntegral i / nd) * range + lmin
    nd = fromIntegral n
    range = lmax - lmin

-- Testing

renderMyCircle :: IO ()
renderMyCircle = renderRasterific "mycircle.png" size myCircle
  where
    size = D.dims (V2 500 500)

myCircle :: Diagram B
myCircle = D.frame 0.05 $ D.circle 1
