{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class (MonadIO)
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
    putStrLn $ concat [ "Number of Stars: ", show . length . inputStarsStars $ inStars ]
    putStrLn $ concat [ "Number Failed:   ", show . inputStarsFailedCount $ inStars ]


data Star
    = Star
    { starRA  :: {-# UNPACK #-} !Float
    , starDec :: {-# UNPACK #-} !Float
    , starMag :: {-# UNPACK #-} !Float
    , starCI  :: {-# UNPACK #-} !Float
    }


data InputStars
    = InputStars
    { inputStarsStars       :: [Star]
    , inputStarsFailedCount :: Int
    }
    

addInputStar :: InputStars -> Maybe Star -> InputStars
addInputStar (InputStars stars count) maybeStar =
    case maybeStar of
        Just star -> InputStars (star : stars)  count
        Nothing   -> InputStars         stars  (count + 1)
    

readStarsFile :: FilePath -> IO InputStars
readStarsFile filePath = withFile filePath ReadMode $ \handle -> do
    S.foldl addInputStar (InputStars [] 0) id
    . S.serially
    . fmap (readLine . T.pack)
    . S.fromHandle
    $ handle


readLine :: Text -> Maybe Star
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
    in
        Star
        <$> lookup 7
        <*> lookup 8
        <*> lookup 13
        <*> lookup 16



-- readStream :: (S.Streaming t) => t m Text ->

{-
toTextStream :: S.Streaming t => t m String -> t m Text
toTextStream = fmap T.pack
-}
