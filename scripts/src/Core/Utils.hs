module Core.Utils where

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