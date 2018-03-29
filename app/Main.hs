{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Read         as T
import qualified Data.Vector            as V
import           GHC.Float              (double2Float)
import qualified Streamly               as S
import qualified Streamly.Prelude       as S
import           System.IO              (IOMode (ReadMode), withFile)

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

        maybeStar :: Maybe Star
        maybeStar =
            Star
            <$> lookup 7
            <*> lookup 8
            <*> lookup 13
            <*> pure (fromMaybe 0 (lookup 16))
    in
        case maybeStar of
            Nothing   -> Left (FailedStarRead line)
            Just star -> Right star
