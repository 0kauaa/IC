module Main where

import Core.Params
import Core.Learner               (Learner(..))
import Models.PolynomialRegressor (polynomialRegressor)
import Training.Training          (step, train, debug)
import Data.Synthetic.Polynomial  (data_03_3)

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
    let pairs = data_03_3
        mu    = mean   (map fst pairs)        
        sigma = stddev (map fst pairs)  

        model = polynomialRegressor mu sigma
        p0    = iniParam model
        ps    = debug model p0 pairs 2000
    putStrLn $ "coeficientes da do polinomio: " ++ show ps    
    putStrLn $ "predicao para a entrada 20: " ++ show (i model ps 20.0)