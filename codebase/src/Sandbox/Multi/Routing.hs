{-# LANGUAGE DataKinds #-}

module Sandbox.Multi.Routing
    ( monoid
    , comonoid
    , swap
    , delete
    , leftUnit
    , rightUnit
    , assoc
    ) where

import Core.MultiLearner (MultiLearner(..))
import Core.Params       (Params(..))
import Core.Multi        (Multi(..))

monoid :: Num a => MultiLearner '[] '[a, a] a
monoid = MultiLearner
    { iM = \ParamsNull (x :-: y :-: MultiNull) -> x + y
    , uM = \ParamsNull _ _                     -> ParamsNull
    , rM = \ParamsNull _ g                     -> g :-: g :-: MultiNull
    , iniParamsM = ParamsNull
    }

comonoid :: Num a => MultiLearner '[] '[a] (a, a)
comonoid = MultiLearner
    { iM = \ParamsNull (x :-: MultiNull)     -> (x, x)
    , uM = \ParamsNull _ _                   -> ParamsNull
    , rM = \ParamsNull _ (g1, g2)            -> g1 + g2 :-: MultiNull
    , iniParamsM = ParamsNull
    }

swap :: MultiLearner '[] '[a, b] (b, a)
swap = MultiLearner
    { iM = \ParamsNull (a :-: b :-: MultiNull) -> (b, a)
    , uM = \ParamsNull _ _                     -> ParamsNull
    , rM = \ParamsNull _ (gb, ga)              -> ga :-: gb :-: MultiNull
    , iniParamsM = ParamsNull
    }

delete :: Num a => MultiLearner '[] '[a] ()
delete = MultiLearner
    { iM = \ParamsNull _   -> ()
    , uM = \ParamsNull _ _ -> ParamsNull
    , rM = \ParamsNull _ _ -> 0 :-: MultiNull
    , iniParamsM = ParamsNull
    }

leftUnit :: MultiLearner '[] '[(), a] a
leftUnit = MultiLearner
    { iM = \ParamsNull (() :-: a :-: MultiNull) -> a
    , uM = \ParamsNull _ _                      -> ParamsNull
    , rM = \ParamsNull _ g                      -> () :-: g :-: MultiNull
    , iniParamsM = ParamsNull
    }

rightUnit :: MultiLearner '[] '[a, ()] a
rightUnit = MultiLearner
    { iM = \ParamsNull (a :-: () :-: MultiNull) -> a
    , uM = \ParamsNull _ _                      -> ParamsNull
    , rM = \ParamsNull _ g                      -> g :-: () :-: MultiNull
    , iniParamsM = ParamsNull
    }

assoc :: MultiLearner '[] '[a, b, c] ((a, b), c)
assoc = MultiLearner
    { iM = \ParamsNull (a :-: b :-: c :-: MultiNull) -> ((a, b), c)
    , uM = \ParamsNull _ _                           -> ParamsNull
    , rM = \ParamsNull _ ((ga, gb), gc)              -> ga :-: gb :-: gc :-: MultiNull
    , iniParamsM = ParamsNull
    }