module Main where

import Core.Params()
import Core.Learner             (Learner(..))
import Models.StandardRegressor (standardlizedRegressor)
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
        ps     = debug model p0 pairs 10
    {-    
    obs.: os parametros mostrados não são normalizadados          
          fica como a fazer, após a publicação do primeiro artigo, uma função 'fromParams' para conversão Params -> Double,
          para possibilitar a normalização dos aprâmetros.    
    -}
    
    putStrLn $ "coeficientes da reta (sem normalização): " ++ show ps ++ "\nmedia: " ++ show mu ++ ", desvio padrao: " ++ show sigma    
    putStrLn $ "predicao para a entrada 17: " ++ show (i model ps 17)