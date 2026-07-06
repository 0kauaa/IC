module Sandbox.Activations
    ( relu
    ) where

import Core.Params
import Core.Learner

relu :: Learner '[] Double Double
relu = Learner
    { i = \ParamsNull x   -> max 0 x
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull x g -> if x > 0 then g else 0
    , iniParam = ParamsNull
    }