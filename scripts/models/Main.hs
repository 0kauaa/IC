{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import LinearRegressor

-- reta original
r :: Double -> Double
r x = x * 2 * x + 1 

-- dados
pairs :: [(Double, Double)]
pairs = [(x, r x) | x <- [1..30]]

main :: IO ()
main = 
    let pairs = pairs
        p0    = iniParam regressor
        ps    = train regressor p0 pairs 30
    
    in
        putStrLn $ "coêficientes da reta: " ++ show ps
        putStrLn $ "predição para a entrada 31: " ++ show (i regressor ps 31.0)