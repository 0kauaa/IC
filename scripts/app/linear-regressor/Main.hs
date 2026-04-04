{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Params()
import Learner (Learner(..))
import Models.LinearRegressor (linearRegressor, step, train, debug)

d :: [(Double, Double)]
d = [(1.0,3.0),(1.3,3.1),     
     (2.1,5.9),(2.0,5.0),
     (5,5.89),(3.0,7.4),
     (4.0,8.0),(5.0,11.71),
     (5.8,12.8),(5.5,9.5),
     (6.0,11.3),(6.1,11.1),
     (7.0,15.0),(8.0,17.0),
     (8.1,17.8),(8.7,19.0),
     (9.0,20.7),(10.0,21.41),
     (10.1,22.88),(11.0,22.3),
     (12.0,24.0),(13.0,27.0),
     (13.6,28.87),(14.0,28.11),
     (15.0,31.18),(16.0,33.2)]

main :: IO ()
main = do
    let pairs = d
        model = linearRegressor
        p0    = iniParam model        
        ps    = train model p0 pairs 1000    
    putStrLn $ "coeficientes da reta: " ++ show ps    
    putStrLn $ "predicao para a entrada 17: " ++ show (i model ps 17.0)