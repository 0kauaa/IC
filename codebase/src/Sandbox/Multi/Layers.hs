{-# LANGUAGE DataKinds #-}

module Sandbox.Multi.Layers
    ( linearMulti
    , denseMulti
    ) where

import Core.MultiLearner (MultiLearner(..))
import Core.Params       (Params(..))
import Core.Multi        (Multi(..))

linearMulti :: MultiLearner '[Double, Double, Double] '[Double, Double] Double
linearMulti = MultiLearner
    { iM = \(w1 ::: w2 ::: b ::: ParamsNull) (x1 :::: x2 :::: MultiNull) ->
                w1*x1 + w2*x2 + b

    , uM = \(w1 ::: w2 ::: b ::: ParamsNull) (x1 :::: x2 :::: MultiNull) g ->
                (w1 - ep*g*x1) ::: (w2 - ep*g*x2) ::: (b - ep*g) ::: ParamsNull

    , rM = \(w1 ::: w2 ::: _ ::: ParamsNull) _ g ->
                (g*w1) :::: (g*w2) :::: MultiNull

    , iniParamsM = 0.0 ::: 0.0 ::: 0.0 ::: ParamsNull
    }
  where ep = 0.01

denseMulti :: [Double] -> Int -> MultiLearner '[[Double]] '[[Double]] Double
denseMulti w0s _ = MultiLearner
    { iM = \(ws ::: ParamsNull) (xs :::: MultiNull) ->
                sum (zipWith (*) (init ws) xs) + last ws
                

    , uM = \(ws ::: ParamsNull) (xs :::: MultiNull) g ->
                let ws'  = zipWith (\wi xi -> wi - ep*g*xi) (init ws) xs
                    b'   = last ws - ep*g
                in (ws' ++ [b']) ::: ParamsNull

    , rM = \(ws ::: ParamsNull) (xs :::: MultiNull) g ->
        let grad = map (* g) (init ws)
        in  grad :::: MultiNull

    , iniParamsM = w0s ::: ParamsNull
    } where ep = 0.01