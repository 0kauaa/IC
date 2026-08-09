module Main where

-- essenciais
import Prelude hiding             (id, (.))
import Core.Cat                   (Cat(..))
import Core.Learner               (Learner(..))
import Core.Params                (Params(..))

-- learners
import Sandbox.Layers             (denseLayer)
import Sandbox.Activations        (relu)
import Sandbox.Outputs            (bceOutput)
import Sandbox.Preprocessing      (zScore)

-- treinamento e teste
import Core.Utils                 (mean, stddev)
import Training.Training          (train, accuracy)

-- dados
import Dataset.Empirical.IrisPCA2 (IrisPCA2(..), fromIrisPCA2)
import Data.Csv                   (decodeByName)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- modelo
layer1 ::Learner '[Double, Double] Double Double
layer1 = denseLayer 0.3 0.0

layer2 ::Learner '[Double, Double] Double Double
layer2 = denseLayer 0.5 0.0

classifier :: Double -> Double -> Learner '[Double, Double, Double, Double] Double Double
classifier mu sigma = bceOutput . layer2 . relu . layer1 . zScore mu sigma

main :: IO ()
main = do

    trainFile <- BL.readFile "../data/iris/prep/iris2_train.csv"
    trainData <- case decodeByName trainFile of
        Left  e      -> error e
        Right (_, v) -> return $ V.toList v

    testFile <- BL.readFile "../data/iris/prep/iris2_test.csv"
    testData <- case decodeByName testFile of
        Left  e      -> error e
        Right (_, v) -> return $ V.toList v
    
    let trainPairs = map fromIrisPCA2 trainData
        testPairs  = map fromIrisPCA2 testData
        mu         = mean   (map fst trainPairs)
        sigma      = stddev (map fst trainPairs)
        model      = classifier mu sigma
        p0         = iniParam model
        ps         = train model p0 trainPairs 100

    putStrLn $ "acuracia: " ++ show (accuracy model ps testPairs * 100) ++ "%"