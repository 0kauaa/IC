module Sandbox.Cat.Preprocessing
    ( zScore
    , minMax
    , binEncoder) where

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

binEncoder :: Double -> Learner '[] Double Double
binEncoder t = Learner
    { i = \ParamsNull x   -> if x >= t then 1 else 0
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull _ _ -> 0
    , iniParam = ParamsNull
    }