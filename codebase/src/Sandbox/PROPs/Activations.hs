{-# LANGUAGE DataKinds #-}

module Sandbox.PROPs.Activations
    ( relu
    , sigmoid
    , tanh
    , softmax
    ) where

import Prelude hiding (tanh)
import qualified Prelude as P

import Core.PROPsLearner (PROPsLearner(..))
import Core.Params       (Params(..))
import Core.Multi        (Multi(..))

relu :: PROPsLearner '[] '[[Double]] '[[Double]]
relu = PROPsLearner
    { iP = \ParamsNull (xs :-: MultiNull) -> map (max 0) xs :-: MultiNull
    , uP = \ParamsNull _ _                -> ParamsNull
    , rP = \ParamsNull (xs :-: MultiNull) (gs :-: MultiNull) ->
            zipWith (\xi gi -> if xi > 0 then gi else 0) xs gs :-: MultiNull
    , iniParamsP = ParamsNull
    }

sigmoid :: PROPsLearner '[] '[[Double]] '[[Double]]
sigmoid = PROPsLearner
    { iP = \ParamsNull (xs :-: MultiNull) -> map sig xs :-: MultiNull
    , uP = \ParamsNull _ _                -> ParamsNull
    , rP = \ParamsNull (xs :-: MultiNull) (gs :-: MultiNull) ->
            zipWith (\xi gi -> let s = sig xi in gi * s * (1 - s)) xs gs :-: MultiNull
    , iniParamsP = ParamsNull
    }
  where
    sig x = 1.0 / (1.0 + exp (-x))

tanh :: PROPsLearner '[] '[[Double]] '[[Double]]
tanh = PROPsLearner
    { iP = \ParamsNull (xs :-: MultiNull) -> map P.tanh xs :-: MultiNull
    , uP = \ParamsNull _ _                -> ParamsNull
    , rP = \ParamsNull (xs :-: MultiNull) (gs :-: MultiNull) ->
            zipWith (\xi gi -> let t = P.tanh xi in gi * (1 - t * t)) xs gs :-: MultiNull
    , iniParamsP = ParamsNull
    }

softmax :: PROPsLearner '[] '[[Double]] '[[Double]]
softmax = PROPsLearner
    { iP = \ParamsNull (zs :-: MultiNull) ->
            let exps = map exp zs
                s    = sum exps
            in  map (/ s) exps :-: MultiNull
    , uP = \ParamsNull _ _ -> ParamsNull
    , rP = \ParamsNull (zs :-: MultiNull) (gs :-: MultiNull) ->
            let exps = map exp zs
                s    = sum exps
                sv   = map (/ s) exps
                prod = sum (zipWith (*) sv gs)
            in  zipWith (\si gi -> si * (gi - prod)) sv gs :-: MultiNull
    , iniParamsP = ParamsNull
    }