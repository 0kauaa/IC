module Sandbox.PROPs.Embed 
    ( fromMulti
    ) where

import Core.Multi        (Multi(..))
import Core.MultiLearner (MultiLearner(..))
import Core.PROPsLearner (PROPsLearner(..))

fromMulti :: MultiLearner ps as b -> PROPsLearner ps as '[b]
fromMulti f = PROPsLearner
    { iP        = \p as  -> iM f p as :-: MultiNull
    , uP        = \p as (b :-: MultiNull) -> uM f p as b
    , rP        = \p as (b :-: MultiNull) -> rM f p as b
    , iniParamsP = iniParamsM f
    }