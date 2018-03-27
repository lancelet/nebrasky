{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.Float (double2Float)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V

main :: IO ()
main = putStrLn "Hello World"


data Star
    = Star
    { starRA  :: {-# UNPACK #-} !Float
    , starDec :: {-# UNPACK #-} !Float
    , starMag :: {-# UNPACK #-} !Float
    , starCI  :: {-# UNPACK #-} !Float
    }


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
