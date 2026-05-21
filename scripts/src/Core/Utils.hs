module Core.Utils where

import Prelude hiding          ((.))
import Core.Cat               ((.))
import Core.Params            (Params(..))
import Core.Learner           (Learner(..))
import Models.LinearRegressor (linearRegressor)
import Models.StandardRegressor (standardlizer)

-- learner regressor normalizado = rl(p, z(x), z(y)), com z(x) = (x - mu) / sigma
standardlizedRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
standardlizedRegressor mu sigma = linearRegressor . standardlizer mu sigma

-- remove parâmetros aprendidos do espaço normalizado
interpret :: Double -> Double -> Params '[Double, Double] -> Params '[Double, Double]
interpret mu sigma (w ::: b ::: ParamsNull) = 
    let w' = (w / sigma) 
        b' = b - (w * mu) / sigma
    in w' ::: b' ::: ParamsNull

-- média
mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- desvio padrão amostral
stddev :: [Double] -> Double
stddev [] = 0
stddev xs =
    let avg      = mean xs
        n        = fromIntegral (length xs)
        variance = sum (map (\x -> (x - avg) ^ (2 :: Int)) xs) / (n - 1)
    in sqrt variance