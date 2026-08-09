module Main where

import Core.MultiLearner     (MultiLearner(..))
import Core.Multi            (Multi(..))
import Sandbox.Multi.Outputs (bceMultiOutput)
import Sandbox.Multi.Embed   (toMulti)
import Sandbox.Preprocessing (zScore)
import Training.Training     (trainMulti, accuracyMulti)

import Data.Csv (decodeByName)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Dataset.Empirical.Iris2 (Iris2(..), fromIris2)

irisClassifier :: MultiLearner '[[Double]] '[[Double]] Double
--                               w1,  w2,  w3,  w4,  b
irisClassifier = bceMultiOutput [0.0, 0.0, 0.0, 0.0, 0.0]

main :: IO ()
main = do

    trainFile <- BL.readFile "../data/iris/prep/iris2_train.csv"
    trainData <- case decodeByName trainFile of
        Left  e      -> error e
        Right (_, v) -> return $ map fromIris2 (V.toList v)

    testFile <- BL.readFile "../data/iris/prep/iris2_test.csv"
    testData <- case decodeByName testFile of
        Left  e      -> error e
        Right (_, v) -> return $ map fromIris2 (V.toList v)
    
    let model      = irisClassifier
        p0         = iniParamsM model
        ps         = trainMulti model p0 trainData 100

    putStrLn $ "parametros iniciais: " ++ show p0
    putStrLn $ "parametros finais: " ++ show ps
    putStrLn $ "acuracia: " ++ show (accuracyMulti model ps testData * 100) ++ "%"