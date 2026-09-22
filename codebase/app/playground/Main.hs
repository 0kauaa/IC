module Main where

import Prelude hiding (id, (.))

import Core.PROPs        (PROPs(..))
import Core.PROPsLearner (PROPsLearner(..))
import Core.Multi        (Multi(..))

import Sandbox.PROPs.Preprocessing (zScore)
import Sandbox.PROPs.Layers        (denseLayer)
import Sandbox.PROPs.Activations   (relu)
import Sandbox.PROPs.Outputs       (bcePROPsOutput)

import Core.Utils        (mean, stddev)
import Training.Training (trainPROPs, accuracyPROPs)

import Data.List (transpose)
import Data.Csv  (decodeByName)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Dataset.Empirical.Banknotes (fromBanknotes)

-- inicialização não nula (aleatória, entre -0.5 e 0.5)
w0 :: Int -> Double
w0 k = sin (fromIntegral (k * 97 + 131)) * 0.5

-- camada escondida 4 -> 16: 16 linhas, cada uma com 4 pesos e 1 bias
escondida :: [[Double]]
escondida = [[w0 (i * 5 + j) | j <- [0..4]] | i <- [0..15]]
    
-- camada de saida 16 -> 1: 17 valores (16 pesos e 1 bias)
saida :: [Double]
saida =  [w0 (80 + k) | k <- [0..16]]

-- mlp 4 -> 16 -> 1
rede :: [[Double]] -> [Double] -> [Double] -> [Double] -> PROPsLearner '[[[Double]], [Double]] '[[Double]] '[[Double]]
rede ws out mu sigma =
    bcePROPsOutput out . relu . denseLayer ws . zScore mu sigma -- bce ∘ relu ∘ dense ∘ zscore

extrai :: Multi '[[Double]] -> [Double]
extrai (xs :-: MultiNull) = xs

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
    
    let cols  = transpose (map extrai (map fst trainData))
        mu    = map mean cols
        sigma = map stddev cols
        model = rede escondida saida mu sigma
        p0    = iniParamsP model
        ps    = trainPROPs model p0 (map toPROPs trainData) 100


    putStrLn $ "acuracia: " ++ show (accuracyPROPs model ps (map toPROPs testData) * 100) ++ "%"

    where toPROPs (xs, y) = (xs, [y] :-: MultiNull)