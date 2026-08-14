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
import Dataset.Empirical.Banknotes (Banknotes(..), fromBanknotes)

classifier :: MultiLearner '[[Double]] '[[Double]] Double
classifier = bceMultiOutput [0.0, 0.0, 0.0, 0.0, 0.0]

main :: IO ()
main = do

    trainFile <- BL.readFile "../data/banknote/prep/bank_train.csv"
    trainData <- case decodeByName trainFile of
        Left  e      -> error e
        Right (_, v) -> return $ map fromBanknotes (V.toList v)

    testFile <- BL.readFile "../data/banknote/prep/bank_test.csv"
    testData <- case decodeByName testFile of
        Left  e      -> error e
        Right (_, v) -> return $ map fromBanknotes (V.toList v)
    
    let model      = classifier
        p0         = iniParamsM model
        ps         = trainMulti model p0 trainData 100

    putStrLn $ "parametros iniciais: " ++ show p0
    putStrLn $ "parametros finais: " ++ show ps
    putStrLn $ "acuracia: " ++ show (accuracyMulti model ps testData * 100) ++ "%"