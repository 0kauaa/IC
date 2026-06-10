module Main where

import Core.Params()
import Core.Learner              (Learner(..))
import Core.Utils                (mean, stddev)
import Models.Net                (smallNet)
import Training.Training         (train)
import Data.Synthetic.Classified (data_0_1)

main :: IO ()
main = do
    let pairs = data_0_1
        mu    = mean   (map fst pairs)
        sigma = stddev (map fst pairs)
        
        model = smallNet mu sigma
        p0    = iniParam model
        ps    = train model p0 pairs 1000

    putStrLn $ "predicao para a entrada 1.2 (1): " ++ show (i model ps 1.2)