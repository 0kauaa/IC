{-# LANGUAGE DataKinds #-}

module Sandbox.Multi.Activations
    ( relu
    , sigmoid
    , tanh
    ) where

import Prelude hiding (tanh)
import qualified Prelude as P

import Core.MultiLearner (MultiLearner(..))
import Core.Params       (Params(..))
import Core.Multi        (Multi(..))

relu :: MultiLearner '[] '[[Double]] [Double]
relu = MultiLearner
    { iM = \ParamsNull (xs :-: MultiNull)  -> map (max 0) xs
    , uM = \ParamsNull _ _                 -> ParamsNull
    , rM = \ParamsNull (xs :-: MultiNull) gs ->
            zipWith (\xi gi -> if xi > 0 then gi else 0) xs gs :-: MultiNull
    , iniParamsM = ParamsNull
    }

sigmoid :: MultiLearner '[] '[[Double]] [Double]
sigmoid = MultiLearner
    { iM = \ParamsNull (xs :-: MultiNull) -> map sig xs
    , uM = \ParamsNull _ _                -> ParamsNull
    , rM = \ParamsNull (xs :-: MultiNull) gs ->
            zipWith (\xi gi -> let s = sig xi in gi * s * (1 - s)) xs gs :-: MultiNull
    , iniParamsM = ParamsNull
    }
  where
    sig x = 1.0 / (1.0 + exp (-x))

tanh :: MultiLearner '[] '[[Double]] [Double]
tanh = MultiLearner
    { iM = \ParamsNull (xs :-: MultiNull) -> map P.tanh xs
    , uM = \ParamsNull _ _                -> ParamsNull
    , rM = \ParamsNull (xs :-: MultiNull) gs ->
            zipWith (\xi gi -> let t = P.tanh xi in gi * (1 - t * t)) xs gs :-: MultiNull
    , iniParamsM = ParamsNull
    }