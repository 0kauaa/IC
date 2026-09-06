module Main where

import Core.PROPsLearner         (PROPsLearner(..))
import Core.Multi                (Multi(..))
import Sandbox.PROPs.Outputs     (bcePROPsOutput)
import Training.Training         (trainPROPs, accuracyPROPs)

import Data.Csv (decodeByName)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Dataset.Empirical.Banknotes (Banknotes(..), fromBanknotes)

classifier :: PROPsLearner '[[Double]] '[[Double]] '[[Double]]
classifier = bcePROPsOutput [0.0, 0.0, 0.0, 0.0, 0.0]

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
        p0         = iniParamsP model
        ps         = trainPROPs model p0 (map toPROPs trainData) 100

    putStrLn $ "parametros iniciais: " ++ show p0
    putStrLn $ "parametros finais: " ++ show ps
    putStrLn $ "acuracia: " ++ show (accuracyPROPs model ps (map toPROPs testData) * 100) ++ "%"
  where
    toPROPs (xs, y) = (xs, [y] :-: MultiNull)