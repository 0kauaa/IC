{-# LANGUAGE DataKinds #-}

module Sandbox.Multi.Preprocessing
    ( zScore
    , minMax
    , binEncoder
    ) where

import Core.MultiLearner (MultiLearner(..))
import Core.Params       (Params(..))
import Core.Multi        (Multi(..))

zScore :: [Double] -> [Double] -> MultiLearner '[] '[[Double]] [Double]
zScore mus sigmas = MultiLearner
    { iM = \ParamsNull (xs :-: MultiNull) ->
            zipWith3 (\mu sig x -> (x - mu) / sig) mus sigmas xs
    , uM = \ParamsNull _ _ -> ParamsNull
    , rM = \ParamsNull _ gs ->
            zipWith (\sig g -> g / sig) sigmas gs :-: MultiNull
    , iniParamsM = ParamsNull
    }

minMax :: [Double] -> [Double] -> MultiLearner '[] '[[Double]] [Double]
minMax lowers highers = MultiLearner
    { iM = \ParamsNull (xs :-: MultiNull) ->
            zipWith3 (\lo hi x -> (x - lo) / (hi - lo)) lowers highers xs
    , uM = \ParamsNull _ _ -> ParamsNull
    , rM = \ParamsNull _ gs ->
            zipWith3 (\lo hi g -> g / (hi - lo)) lowers highers gs :-: MultiNull
    , iniParamsM = ParamsNull
    }

binEncoder :: Double -> MultiLearner '[] '[[Double]] [Double]
binEncoder t = MultiLearner
    { iM = \ParamsNull (xs :-: MultiNull) -> map (\x -> if x >= t then 1 else 0) xs
    , uM = \ParamsNull _ _                -> ParamsNull
    , rM = \ParamsNull (xs :-: MultiNull) _ -> map (const 0) xs :-: MultiNull
    , iniParamsM = ParamsNull
    }