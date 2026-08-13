{-# LANGUAGE DataKinds #-}

module Sandbox.Multi.Outputs
    ( mseMultiOutput
    , bceMultiOutput
    ) where

import Core.MultiLearner (MultiLearner(..))
import Core.Params       (Params(..))
import Core.Multi        (Multi(..))

mseMultiOutput :: [Double] -> MultiLearner '[[Double]] '[[Double]] Double
mseMultiOutput w0s = MultiLearner
    { iM = \(ws :|: ParamsNull) (xs :-: MultiNull) ->
                sum (zipWith (*) (init ws) xs) + last ws

    , uM = \(ws :|: ParamsNull) (xs :-: MultiNull) y ->
                let yhat = sum (zipWith (*) (init ws) xs) + last ws
                    err  = yhat - y
                    ws'  = zipWith (\wi xi -> wi - ep*err*xi) (init ws) xs
                    b'   = last ws - ep*err
                in (ws' ++ [b']) :|: ParamsNull

    , rM = \(ws :|: ParamsNull) (xs :-: MultiNull) y ->
                let yhat = sum (zipWith (*) (init ws) xs) + last ws
                    err  = yhat - y
                in map (* err) (init ws) :-: MultiNull

    , iniParamsM = w0s :|: ParamsNull
    } where ep = 0.01

bceMultiOutput :: [Double] -> MultiLearner '[[Double]] '[[Double]] Double
bceMultiOutput w0s = MultiLearner
    { iM = \(ws :|: ParamsNull) (xs :-: MultiNull) ->
                let z = sum (zipWith (*) (init ws) xs) + last ws
                in 1.0 / (1.0 + exp (-z))

    , uM = \(ws :|: ParamsNull) (xs :-: MultiNull) y ->
                let z    = sum (zipWith (*) (init ws) xs) + last ws
                    s    = 1.0 / (1.0 + exp (-z))
                    grad = s - y
                    ws'  = zipWith (\wi xi -> wi - ep*grad*xi) (init ws) xs
                    b'   = last ws - ep*grad
                in (ws' ++ [b']) :|: ParamsNull

    , rM = \(ws :|: ParamsNull) (xs :-: MultiNull) y ->
                let z    = sum (zipWith (*) (init ws) xs) + last ws
                    s    = 1.0 / (1.0 + exp (-z))
                    grad = s - y
                in map (* grad) (init ws) :-: MultiNull

    , iniParamsM = w0s :|: ParamsNull
    } where ep = 0.01