{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Params()
import Learner (Learner(..))
import LinearRegressor

-- reta original
f :: Double -> Double
f x = x * 2 + 1 

main :: IO ()
main = do
    let pairs = [(x, f x) | x <- [1..30] :: [Double]]
        p0    = iniParam regressor
        ps    = debug regressor p0 pairs 1000
    
    putStrLn $ "coeficientes da reta: " ++ show ps
    putStrLn $ "predicao para a entrada 31: " ++ show (i regressor ps 31.0)