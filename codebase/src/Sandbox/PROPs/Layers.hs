{-# LANGUAGE DataKinds #-}

module Sandbox.PROPs.Layers
    ( linear
    , denseLayer
    ) where

import Data.List (transpose)

import Core.PROPsLearner (PROPsLearner(..))
import Core.Multi        (Multi(..))
import Core.Params       (Params(..))

linear :: PROPsLearner '[Double, Double, Double] '[Double, Double] '[Double]
linear = PROPsLearner
    { iP = \(w1 :|: w2 :|: b :|: ParamsNull) (x1 :-: x2 :-: MultiNull) ->
                (w1*x1 + w2*x2 + b) :-: MultiNull

    , uP = \(w1 :|: w2 :|: b :|: ParamsNull) (x1 :-: x2 :-: MultiNull)
            (g :-: MultiNull) ->
                (w1 - ep*g*x1) :|: (w2 - ep*g*x2) :|: (b - ep*g) :|: ParamsNull

    , rP = \(w1 :|: w2 :|: _ :|: ParamsNull) _ (g :-: MultiNull) ->
                (g*w1) :-: (g*w2) :-: MultiNull

    , iniParamsP = 0.0 :|: 0.0 :|: 0.0 :|: ParamsNull
    }
  where ep = 0.01

denseLayer :: [[Double]] -> PROPsLearner '[[[Double]]] '[[Double]] '[[Double]]
denseLayer w0ss = PROPsLearner
    { iP = \(wss :|: ParamsNull) (xs :-: MultiNull) ->
            map (lin xs) wss :-: MultiNull
    , uP = \(wss :|: ParamsNull) (xs :-: MultiNull) (gs :-: MultiNull) ->
            let wss' = zipWith (updRow xs) wss gs
            in  wss' :|: ParamsNull
    , rP = \(wss :|: ParamsNull) _ (gs :-: MultiNull) ->
            let wT = transpose (map init wss)
            in  map (sum . zipWith (*) gs) wT :-: MultiNull
    , iniParamsP = w0ss :|: ParamsNull
    }
  where
    lin wi xs = sum (zipWith (*) (init wi) xs) + last wi
    updRow xs wi gi =
        let ws' = zipWith (\w xj -> w - ep * gi * xj) (init wi) xs
            b'  = last wi - ep * gi
        in  ws' ++ [b']
    ep = 0.01