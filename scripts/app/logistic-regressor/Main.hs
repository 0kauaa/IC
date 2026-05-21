module Main where

import Core.Params()
import Core.Learner              (Learner(..))
import Core.Utils                (mean, stddev)
import Models.LogisticRegressor  (logisticRegressor)
import Training.Training         (step, train, debug)
import Data.Synthetic.Classified (data_0_1)

main :: IO ()
main = do
    let pairs = data_0_1
        model = logisticRegressor
        p0    = iniParam model    
        ps    = train model p0 pairs 1000
    putStrLn $ "coeficientes de classificacao: " ++ show ps
    putStrLn $ "predicao para a entrada 17: " ++ show (i model ps 0.0)