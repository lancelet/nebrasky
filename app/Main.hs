{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad                   (forM_)
import           Control.Monad.Primitive         (PrimMonad, PrimState)
import           Control.Monad.ST                (runST)
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Read                  as T
import qualified Data.Vector                     as V
import qualified Data.Vector.Unboxed             as U
import           GHC.Float                       (double2Float)
import           Linear.V2                       (V2 (V2))
import qualified Streamly                        as S
import qualified Streamly.Prelude                as S
import           System.IO                       (IOMode (ReadMode), withFile)
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWC

main :: IO ()
main = do
    putStrLn "Hello World"
    inStars <- readStarsFile "data/HYG-Database/hygdata_v3.csv"
    putStrLn $ concat [ "Number of Stars: ", show . length . inputStarsStars  $ inStars ]
    putStrLn $ concat [ "Number Failed:   ", show . length . inputStarsFailed $ inStars ]

    forM_ (inputStarsFailed inStars) $ \(FailedStarRead txt) ->
        putStrLn . T.unpack $ txt


data Star
    = Star
    { starRA  :: {-# UNPACK #-} !Float
    , starDec :: {-# UNPACK #-} !Float
    , starMag :: {-# UNPACK #-} !Float
    , starCI  :: {-# UNPACK #-} !Float
    }


data InputStars
    = InputStars
    { inputStarsStars  :: [Star]
    , inputStarsFailed :: [FailedStarRead]
    }

newtype FailedStarRead = FailedStarRead Text


addInputStar :: InputStars -> Either FailedStarRead Star -> InputStars
addInputStar (InputStars stars failed) eitherStar =
    case eitherStar of
        Right star   -> InputStars (star : stars)            failed
        Left notRead -> InputStars         stars  (notRead : failed)


readStarsFile :: FilePath -> IO InputStars
readStarsFile filePath = withFile filePath ReadMode $ \handle -> do
    S.foldl addInputStar (InputStars [] []) id
    . S.serially
    . fmap (readLine . T.pack)
    . S.fromHandle
    $ handle


readLine :: Text -> Either FailedStarRead Star
readLine line =
    let
        items :: V.Vector Text
        items = V.fromList (T.splitOn "," line)

        toFloat :: Text -> Maybe Float
        toFloat t =
            case T.double t of
                Left _        -> Nothing
                Right (x, "") -> Just (double2Float x)

        lookup :: Int -> Maybe Float
        lookup i = items V.!? i >>= toFloat

        lookupWithDefault :: Int -> Float -> Maybe Float
        lookupWithDefault i x = Just (fromMaybe x (lookup i))

        maybeStar :: Maybe Star
        maybeStar =
            Star
            <$> lookup 7
            <*> lookup 8
            <*> lookup 13
            <*> lookupWithDefault 16 0
    in
        case maybeStar of
            Nothing   -> Left (FailedStarRead line)
            Just star -> Right star


{-
TODO:

Kensler A (2013) Correlated Multi-Jittered Sampling. Pixar Technical Memo 13-01.

https://graphics.pixar.com/library/MultiJitteredSampling/paper.pdf
-}

type NSamplesX = Int
type NSamplesY = Int

pixelSamples :: MWC.Seed -> NSamplesX -> NSamplesY -> U.Vector (V2 Float)
pixelSamples seed nx ny = runST $ do
    gen <- MWC.restore seed
    pixelSamplesM gen nx ny

pixelSamplesM
    :: ( Monad m
       , PrimMonad m
       )
    => MWC.Gen (PrimState m)
    -> NSamplesX
    -> NSamplesY
    -> m (U.Vector (V2 Float))
pixelSamplesM gen nx ny = do
    xs :: U.Vector Int <- MWC.uniformPermutation nx gen
    ys :: U.Vector Int <- MWC.uniformPermutation ny gen
    U.generateM (nx * ny) $ \idx ->
        let
            (xi, yi) = quotRem idx nx
            xi'      = xs U.! xi
            yi'      = ys U.! yi
        in
            nRook gen nx ny (xi', yi')

nRook
    :: ( Monad m
       , PrimMonad m
       )
    => MWC.Gen (PrimState m)
    -> NSamplesX
    -> NSamplesY
    -> (Int, Int)
    -> m (V2 Float)
nRook gen nx ny (xx, yy) = do
    x :: Float <- MWC.uniform gen
    y :: Float <- MWC.uniform gen
    let
        x' = fromIntegral xx + x / fromIntegral nx
        y' = fromIntegral yy + y / fromIntegral ny
    pure (V2 x' y')
