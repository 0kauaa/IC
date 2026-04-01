{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Params()
import Learner (Learner(..))
import LinearRegressor

-- reta original
line :: Double -> Double
line x = x * 2 * x + 1 

main :: IO ()
main = do
    let pairs = [(x, line x) | x <- [1..30]]
        p0    = iniParam regressor
        ps    = train regressor p0 pairs 30
    
    putStrLn $ "coeficientes da reta: " ++ show ps
    putStrLn $ "predicao para a entrada 31: " ++ show (i regressor ps 31.0)