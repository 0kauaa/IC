module Main where

import Core.Params()
import Core.Learner             (Learner(..))
import Models.StandardRegressor (standardlizedRegressor, interpret)
import Training.Training        (step, train, debug)
import Data.Synthetic.Linear    (data_2_1)

-- média
mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- desvio padrao amostral
stddev :: [Double] -> Double
stddev [] = 0
stddev xs =
    let avg      = mean xs        
        n        = fromIntegral (length xs)        
        variance = sum (map (\x -> (x - avg) ^ (2 :: Int)) xs) / (n - 1)    
    in sqrt variance
    
main :: IO ()
main = do    
    let pairs  = data_2_1
        mu     = mean   (map fst pairs)        
        sigma  = stddev (map fst pairs)         
               
        model  = standardlizedRegressor mu sigma        
        p0     = iniParam model        
        ps     = debug model p0 pairs 1000

    putStrLn $ "coeficientes da reta: " ++ show (interpret mu sigma ps) ++ "\nmedia: " ++ show mu ++ ", desvio padrao: " ++ show sigma    
    putStrLn $ "predicao para a entrada 17: " ++ show (i model ps 17)