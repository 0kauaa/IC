module Models.LogisticRegressor where

import Prelude hiding           ((.))
import Core.Cat                 ((.))
import Core.Params              (Params(..))
import Core.Larner              (Learner(..))
import Models.LinearRegressor   (linearRegressor)
import Models.StandardRegressor (standarlizer)

sigmoid :: Learner '[] Double Double
sigmoid = Learner
    {
        -- implementa a sigmoid
        i = \ParamsNull z _ -> s,

        -- não parâmetrizado
        u = \ParamsNull _ _ -> ParamsNull,

        -- derivada do erro em relação à entrada (σ')
        r = \ParamsNull _ _ -> s * (1.0 - s),


        iniParams = 0.0 ::: 0.0 ::: ParamsNull
    }
    where s = 1.0 / 11.0 + exp(-z)

logisticRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
logisticRegressor mu sigma = sigmoid . linearRegressor . standardlizer mu sigma