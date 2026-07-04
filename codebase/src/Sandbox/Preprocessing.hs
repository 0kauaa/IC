module Sandbox.Preprocessing (zScore, minMax) where

import Core.Params
import Core.Learner

zScore :: Double -> Double -> Learner '[] Double Double
zScore mu sigma = Learner
    { i = \ParamsNull x   -> (x - mu) / sigma
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull _ z -> z / sigma
    , iniParam = ParamsNull
    }

minMax :: Double -> Double -> Learner '[] Double Double
minMax lower higher = Learner
    { i = \ParamsNull x   -> (x - lower) / (higher - lower)
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull _ g -> g / (higher - lower)
    , iniParam = ParamsNull
    }