{-# LANGUAGE DataKinds #-}

module Sandbox.PROPs.Outputs
    ( msePROPsOutput
    , bcePROPsOutput
    , ccePROPsOutput
    , softmaxPROPsOutput
    ) where

import Data.List (transpose)

import Core.PROPsLearner (PROPsLearner(..))
import Core.Params       (Params(..))
import Core.Multi        (Multi(..))

msePROPsOutput :: [Double] -> PROPsLearner '[[Double]] '[[Double]] '[[Double]]
msePROPsOutput w0s = PROPsLearner
    { iP = \(ws :|: ParamsNull) (xs :-: MultiNull) ->
            [lin ws xs] :-: MultiNull
    , uP = \(ws :|: ParamsNull) (xs :-: MultiNull) (ys :-: MultiNull) ->
            let err  = lin ws xs - head ys
                ws'  = zipWith (\wi xi -> wi - ep * err * xi) (init ws) xs
                b'   = last ws - ep * err
            in  (ws' ++ [b']) :|: ParamsNull
    , rP = \(ws :|: ParamsNull) (xs :-: MultiNull) (ys :-: MultiNull) ->
            let err = lin ws xs - head ys
            in  map (* err) (init ws) :-: MultiNull
    , iniParamsP = w0s :|: ParamsNull
    }
  where
    lin ws xs = sum (zipWith (*) (init ws) xs) + last ws
    ep = 0.01

bcePROPsOutput :: [Double] -> PROPsLearner '[[Double]] '[[Double]] '[[Double]]
bcePROPsOutput w0s = PROPsLearner
    { iP = \(ws :|: ParamsNull) (xs :-: MultiNull) ->
            [sig (lin ws xs)] :-: MultiNull
    , uP = \(ws :|: ParamsNull) (xs :-: MultiNull) (ys :-: MultiNull) ->
            let z    = lin ws xs
                s    = sig z
                grad = s - head ys
                ws'  = zipWith (\wi xi -> wi - ep * grad * xi) (init ws) xs
                b'   = last ws - ep * grad
            in  (ws' ++ [b']) :|: ParamsNull
    , rP = \(ws :|: ParamsNull) (xs :-: MultiNull) (ys :-: MultiNull) ->
            let z    = lin ws xs
                s    = sig z
                grad = s - head ys
            in  map (* grad) (init ws) :-: MultiNull
    , iniParamsP = w0s :|: ParamsNull
    }
  where
    lin ws xs = sum (zipWith (*) (init ws) xs) + last ws
    sig z     = 1.0 / (1.0 + exp (-z))
    ep = 0.01

ccePROPsOutput :: [[Double]] -> PROPsLearner '[[[Double]]] '[[Double]] '[[Double]]
ccePROPsOutput w0ss = PROPsLearner
    { iP = \(wss :|: ParamsNull) (xs :-: MultiNull) ->
            softmaxOf (map (`lin` xs) wss) :-: MultiNull
    , uP = \(wss :|: ParamsNull) (xs :-: MultiNull) (ys :-: MultiNull) ->
            let err   = zipWith (-) (softmaxOf (map (`lin` xs) wss)) ys
                wss'  = zipWith (updRow xs) wss err
            in  wss' :|: ParamsNull
    , rP = \(wss :|: ParamsNull) (xs :-: MultiNull) (ys :-: MultiNull) ->
            let err = zipWith (-) (softmaxOf (map (`lin` xs) wss)) ys
                wT  = transpose (map init wss)
            in  map (sum . zipWith (*) err) wT :-: MultiNull
    , iniParamsP = w0ss :|: ParamsNull
    }
  where
    lin wi xs = sum (zipWith (*) (init wi) xs) + last wi
    softmaxOf zs =
        let exps = map exp zs
            s    = sum exps
        in  map (/ s) exps
    updRow xs wi errk =
        let ws' = zipWith (\w xj -> w - ep * errk * xj) (init wi) xs
            b'  = last wi - ep * errk
        in  ws' ++ [b']
    ep = 0.01

softmaxPROPsOutput :: [[Double]] -> PROPsLearner '[[[Double]]] '[[Double]] '[[Double]]
softmaxPROPsOutput w0ss = PROPsLearner
    { iP = \(wss :|: ParamsNull) (xs :-: MultiNull) ->
            softmaxOf (map (`lin` xs) wss) :-: MultiNull
    , uP = \(wss :|: ParamsNull) (xs :-: MultiNull) (gs :-: MultiNull) ->
            let dz    = jacobian (softmaxOf (map (`lin` xs) wss)) gs
                wss'  = zipWith (updRow xs) wss dz
            in  wss' :|: ParamsNull
    , rP = \(wss :|: ParamsNull) (xs :-: MultiNull) (gs :-: MultiNull) ->
            let dz = jacobian (softmaxOf (map (`lin` xs) wss)) gs
                wT = transpose (map init wss)
            in  map (sum . zipWith (*) dz) wT :-: MultiNull
    , iniParamsP = w0ss :|: ParamsNull
    }
  where
    lin wi xs = sum (zipWith (*) (init wi) xs) + last wi
    softmaxOf zs =
        let exps = map exp zs
            s    = sum exps
        in  map (/ s) exps
    jacobian sv gs =
        let prod = sum (zipWith (*) sv gs)
        in  zipWith (\si gi -> si * (gi - prod)) sv gs
    updRow xs wi dzk =
        let ws' = zipWith (\w xj -> w - ep * dzk * xj) (init wi) xs
            b'  = last wi - ep * dzk
        in  ws' ++ [b']
    ep = 0.01