module Main where

import Core.Params                (Params(..))
import Core.Learner               (Learner(..))
import Models.StandardRegressor   (interpret)
import Models.PolynomialRegressor (polynomialRegressor)
import Training.Training          (step, train, debug)
import Data.Synthetic.Polynomial  (data_03_3neg)

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
    let pairs = data_03_3neg
        xs2   = map (\(x, _) -> x*x) pairs -- os parâmetros de padronização devem ser dos dados quadrados
        mu    = mean   xs2
        sigma = stddev xs2  

        model = polynomialRegressor mu sigma
        p0    = iniParam model
        ps    = debug model p0 pairs 200
    putStrLn $ "coeficientes da do polinomio: " ++ show (interpret mu sigma ps)
    putStrLn $ "predicao para a entrada 20: " ++ show (i model ps 20.0)