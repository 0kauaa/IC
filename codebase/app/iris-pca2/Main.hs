{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Csv (decodeByName)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Dataset.Empirical.IrisPCA2 (IrisPCA2(..), fromIrisPCA2)

main :: IO ()
main = do
    iris <- BL.readFile "../data/iris/prep/iris2_test.csv"

    case decodeByName iris of
        Left  e      -> putStrLn e
        Right (_, v) -> do
            let pairs = V.toList (V.map fromIrisPCA2 v)
            
            print pairs