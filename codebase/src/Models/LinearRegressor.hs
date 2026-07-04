{-# LANGUAGE TypeFamilies #-}

module Models.LinearRegressor(linearRegressor) where

import Core.Learner
import Core.Params  (Params(..))

linearRegressor :: Learner '[Double, Double] Double Double
linearRegressor = Learner
    {
        i = \(w ::: b ::: ParamsNull) x ->
            x * w + b,

        u = \(w ::: b ::: ParamsNull) x y ->
            let ŷ  = (x * w + b)
                e  = ŷ - y
                w' = w - ep * e * x
                b' = b - ep * e
                in w' ::: b' ::: ParamsNull,

        r = \(w ::: b ::: ParamsNull) x y ->
            let ŷ = (x * w + b)
                e = ŷ - y
                in x - ep * e * w,
                
        iniParam = 0.0 ::: 0.0 ::: ParamsNull
    }
    where ep = 0.001