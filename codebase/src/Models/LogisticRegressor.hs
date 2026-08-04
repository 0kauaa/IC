module Models.LogisticRegressor 
    ( sigmoid
    , logisticRegressor) where

import Prelude hiding           ((.))
import Core.Cat                 ((.))
import Core.Params              (Params(..))
import Core.Learner             (Learner(..))
import Models.StandardRegressor (standardlizer)

linearRegressor :: Learner '[Double, Double] Double Double
linearRegressor = Learner
    { i = \(w ::: b ::: ParamsNull) x   -> x * w + b
    , u = \(w ::: b ::: ParamsNull) z p ->
        let w' = w - ep * p * z
            b' = b - ep * p
        in  w' ::: b' ::: ParamsNull
    , r = \(w ::: _ ::: ParamsNull) z p -> z - ep * p * w
    , iniParam = 0.0 ::: 0.0 ::: ParamsNull
    } where ep = 0.01

sigmoid :: Learner '[] Double Double
sigmoid = Learner
    { i = \ParamsNull z   -> 1.0 / (1.0 + exp (-z))
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull z y -> 
        let s = 1.0 / (1.0 + exp (-z))
        in  s - y
    , iniParam = ParamsNull
    }

logisticRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
logisticRegressor mu sigma = sigmoid . linearRegressor . standardlizer mu sigma