module Sandbox.Cat.Activations
    ( relu
    , sigmoid
    , tanh
    ) where

import Prelude hiding (tanh)
import qualified Prelude as P

import Core.Params
import Core.Learner

relu :: Learner '[] Double Double
relu = Learner
    { i = \ParamsNull x   -> max 0 x
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull x g -> if x > 0 then g else 0
    , iniParam = ParamsNull
    }

sigmoid :: Learner '[] Double Double
sigmoid = Learner
    { i = \ParamsNull z   -> 1.0 / (1.0 + exp (-z))
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull z g ->
        let s = 1.0 / (1.0 + exp (-z))
        in g * s * (1 - s)
    , iniParam = ParamsNull
    }

tanh :: Learner '[] Double Double
tanh = Learner
    { i = \ParamsNull x   -> P.tanh x
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull x g ->
        let t = P.tanh x
        in g * (1 - t * t)
    , iniParam = ParamsNull
    }