module Main where

import Core.Params()
import Core.Learner           (Learner(..))
import Models.LinearRegressor (linearRegressor)
import Training.Training      (step, train, debug)
import Data.Synthetic.Linear  (data_2_1)

main :: IO ()
main = do
    let pairs = data_2_1
        model = linearRegressor
        p0    = iniParam model        
        ps    = train model p0 pairs 1000
    putStrLn $ "coeficientes da reta: " ++ show ps    
    putStrLn $ "predicao para a entrada 17: " ++ show (i model ps 17.0)