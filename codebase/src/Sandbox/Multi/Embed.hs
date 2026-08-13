{-# LANGUAGE DataKinds #-}

module Sandbox.Multi.Embed (toMulti, firstInput) where

import Core.Learner      (Learner(..))
import Core.MultiLearner (MultiLearner(..))
import Core.Params       (Params(..))
import Core.Multi        (Multi(..))

toMulti :: Learner ps a b -> MultiLearner ps '[a] b
toMulti f = MultiLearner
    { iM       = \p (a :-: MultiNull)   -> i f p a
    , uM       = \p (a :-: MultiNull) b -> u f p a b
    , rM       = \p (a :-: MultiNull) b -> r f p a b :-: MultiNull
    , iniParamsM = iniParam f
    }

firstInput :: Multi '[a] -> a
firstInput (a :-: MultiNull) = a