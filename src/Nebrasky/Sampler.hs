{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
module Nebrasky.Sampler where

import Data.Monoid (mconcat)
import           Diagrams.Backend.Rasterific (B, renderRasterific)
import           Diagrams.Prelude            (Diagram, (#))
import qualified Diagrams.Prelude            as D
import           Linear.V2                   (V2 (V2))

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
