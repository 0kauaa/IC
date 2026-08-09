module Main where

import Core.Params()
import Core.Learner               (Learner(..))
import Core.Utils                 (mean, stddev)
import Models.Net                 (smallNet)
import Training.Training          (train, accuracy)
import Dataset.Empirical.IrisPCA2 (IrisPCA2(..), fromIrisPCA2)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V 
import Data.Csv                   (decodeByName)

main :: IO ()
main = do

    trainFile <- BL.readFile "../data/iris/prep/iris_pca2_train.csv"
    trainData <- case decodeByName trainFile of
        Left  e      -> error e
        Right (_, v) -> return $ V.toList v

    testFile <- BL.readFile "../data/iris/prep/iris_pca2_test.csv"
    testData <- case decodeByName testFile of
        Left  e      -> error e
        Right (_, v) -> return $ V.toList v
    
    let trainPairs = map fromIrisPCA2 trainData
        testPairs  = map fromIrisPCA2 testData
        mu         = mean   (map fst trainPairs)
        sigma      = stddev (map fst trainPairs)
        model      = smallNet mu sigma
        p0         = iniParam model
        ps         = train model p0 trainPairs 100

    putStrLn $ "acuracia: " ++ show (accuracy model ps testPairs * 100) ++ "%"