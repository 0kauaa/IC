module Sandbox.Cat.Outputs
    ( mseOutput
    , bceOutput) where

import Core.Params
import Core.Learner

mseOutput :: Learner '[] Double Double
mseOutput = Learner
    { i        = \ParamsNull x      -> x
    , u        = \ParamsNull _ _    -> ParamsNull
    , r        = \ParamsNull yhat y -> yhat - y
    , iniParam = ParamsNull
    }

bceOutput :: Learner '[] Double Double
bceOutput = Learner
    { i = \ParamsNull z   -> 1.0 / (1.0 + exp (-z))
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull z y -> 1.0 / (1.0 + exp (-z)) - y
    , iniParam = ParamsNull
    }