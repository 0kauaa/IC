module Main where

import Prelude hiding (id, (.))

import Core.PROPs         (PROPs(..))
import Core.PROPsLearner  (PROPsLearner(..))
import Core.Params        (Params(..))
import Core.Multi         (Multi(..))
import Core.Utils         (oneHot)

import Sandbox.PROPs.Layers      (denseLayer)
import Sandbox.PROPs.Activations (relu)
import Sandbox.PROPs.Outputs     (ccePROPsOutput)

import Training.Training  (trainPROPs, accuracyClasses)

import Data.Csv           (decodeByName)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Dataset.Empirical.Iris (fromIris)

-- inicialização determinística não nula (entre -0.5 e 0.5)
w0 :: Int -> Double
w0 k = sin (fromIntegral (k * 97 + 131)) * 0.5

-- camada escondida 4 -> 8: 8 linhas, cada uma com 4 pesos e 1 bias
escondida :: [[Double]]
escondida = [[w0 (i * 5 + j) | j <- [0..4]] | i <- [0..7]]

-- camada de saída 8 -> 3: 3 linhas, cada uma com 8 pesos e 1 bias
saida :: [[Double]]
saida = [[w0 (40 + k * 9 + j) | j <- [0..8]] | k <- [0..2]]

-- mlp 4 -> 8 -> 3
rede :: [[Double]] -> [[Double]] -> PROPsLearner '[[[Double]], [[Double]]] '[[Double]] '[[Double]]
rede ws out = ccePROPsOutput out . relu . denseLayer ws -- cce ∘ relu ∘ dense

main :: IO ()
main = do
    trainFile <- BL.readFile "../data/iris/prep/iris_train.csv"
    trainData <- case decodeByName trainFile of
        Left  e      -> error e
        Right (_, v) -> return $ map fromIris (V.toList v)

    testFile <- BL.readFile "../data/iris/prep/iris_test.csv"
    testData <- case decodeByName testFile of
        Left  e      -> error e
        Right (_, v) -> return $ map fromIris (V.toList v)

    let model = rede escondida saida
        p0    = iniParamsP model
        ps    = trainPROPs model p0 (map toPROPs trainData) 200

    putStrLn $ "acuracia: " ++ show (accuracyClasses model ps testData * 100) ++ "%"
  where
    toPROPs (xs, k) = (xs, oneHot k :-: MultiNull)
