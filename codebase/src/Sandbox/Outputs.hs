module Sandbox.Outputs (bceOutput) where

import Core.Params
import Core.Learner

bceOutput :: Learner '[] Double Double
bceOutput = Learner
    { i = \ParamsNull z   -> 1.0 / (1.0 + exp (-z))
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull z y -> let s = 1.0 / (1.0 + exp (-z)) in  s - y
    , iniParam = ParamsNull
    }