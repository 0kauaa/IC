{-# LANGUAGE DataKinds #-}

module Sandbox.PROPs.Preprocessing
    ( zScore
    , minMax
    , pca
    , multiEncoder
    , binEncoder
    ) where

import Data.List (transpose)

import Core.PROPsLearner (PROPsLearner(..))
import Core.Params       (Params(..))
import Core.Multi        (Multi(..))

zScore :: [Double] -> [Double] -> PROPsLearner '[] '[[Double]] '[[Double]]
zScore mus sigmas = PROPsLearner
    { iP = \ParamsNull (xs :-: MultiNull) ->
            zipWith3 (\mu sig x -> (x - mu) / sig) mus sigmas xs :-: MultiNull
    , uP = \ParamsNull _ _ -> ParamsNull
    , rP = \ParamsNull _ (gs :-: MultiNull) ->
            zipWith (\sig g -> g / sig) sigmas gs :-: MultiNull
    , iniParamsP = ParamsNull
    }

minMax :: [Double] -> [Double] -> PROPsLearner '[] '[[Double]] '[[Double]]
minMax lowers highers = PROPsLearner
    { iP = \ParamsNull (xs :-: MultiNull) ->
            zipWith3 (\lo hi x -> (x - lo) / (hi - lo)) lowers highers xs :-: MultiNull
    , uP = \ParamsNull _ _ -> ParamsNull
    , rP = \ParamsNull _ (gs :-: MultiNull) ->
            zipWith3 (\lo hi g -> g / (hi - lo)) lowers highers gs :-: MultiNull
    , iniParamsP = ParamsNull
    }

pca :: [[Double]] -> PROPsLearner '[] '[[Double]] '[[Double]]
pca wss = PROPsLearner
    { iP = \ParamsNull (xs :-: MultiNull) ->
            map (sum . zipWith (*) xs) wss :-: MultiNull
    , uP = \ParamsNull _ _ -> ParamsNull
    , rP = \ParamsNull _ (gs :-: MultiNull) ->
            let wT = transpose wss
            in  map (sum . zipWith (*) gs) wT :-: MultiNull
    , iniParamsP = ParamsNull
    }

multiEncoder :: Int -> PROPsLearner '[] '[Int] '[[Double]]
multiEncoder k = PROPsLearner
    { iP = \ParamsNull (c :-: MultiNull) ->
            [ if i == c then 1.0 else 0.0 | i <- [0 .. k - 1] ] :-: MultiNull
    , uP = \ParamsNull _ _ -> ParamsNull
    , rP = \ParamsNull _ _ -> 0 :-: MultiNull
    , iniParamsP = ParamsNull
    }

binEncoder :: Double -> PROPsLearner '[] '[[Double]] '[[Double]]
binEncoder t = PROPsLearner
    { iP = \ParamsNull (xs :-: MultiNull) ->
            map (\x -> if x >= t then 1.0 else 0.0) xs :-: MultiNull
    , uP = \ParamsNull _ _ -> ParamsNull
    , rP = \ParamsNull (xs :-: MultiNull) _ ->
            map (const 0) xs :-: MultiNull
    , iniParamsP = ParamsNull
    }