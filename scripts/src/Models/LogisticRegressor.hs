module Models.LogisticRegressor where

import Prelude hiding           ((.))
import Core.Cat                 ((.))
import Core.Params              (Params(..))
import Core.Learner             (Learner(..))
import Models.LinearRegressor   (linearRegressor)
import Models.StandardRegressor (standardlizer)

sigmoid :: Learner '[] Double Double
sigmoid = Learner
    {
        -- implementa a sigmoid
        i = \ParamsNull z -> 1.0 / (1.0 + exp (-z)),

        -- não parâmetrizado
        u = \ParamsNull _ _ -> ParamsNull,

        -- derivada do erro em relação à entrada (σ')
        r = \ParamsNull x _ -> 
            let s = 1.0 / (1.0 + exp (-x))
            in s * (1.0 - s),

        iniParam = ParamsNull
    }

logisticRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
logisticRegressor mu sigma = sigmoid . linearRegressor . standardlizer mu sigma