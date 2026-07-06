module Sandbox.Routing
    ( monoid
    , comonoid
    , delete
    , leftUnit
    , rightUnit
    , assoc) where

import Prelude hiding (id, (.))
import Data.Kind      (Type)
import Core.Learner   (Learner(..))
import Core.Params    (Params(..))

monoid :: Num a => Learner '[] (a, a) a
monoid = Learner
    { i = \ParamsNull (a1, a2) -> a1 + a2
    , u = \ParamsNull _ _      -> ParamsNull
    , r = \ParamsNull _ g      -> (g, g)
    , iniParam = ParamsNull
    }

comonoid :: Num a => Learner '[] a (a, a)
comonoid = Learner
    { i  = \ParamsNull a         -> (a, a)
    , u = \ParamsNull _ _        -> ParamsNull
    , r = \ParamsNull _ (g1, g2) -> g1 + g2
    , iniParam = ParamsNull
    }

swap :: Learner '[] (a, b) (b, a)
swap = Learner
    { i        = \ParamsNull (a, b)      -> (b, a)
    , u        = \ParamsNull _ _         -> ParamsNull
    , r        = \ParamsNull _ (gb, ga)  -> (ga, gb)
    , iniParam = ParamsNull
    }

delete :: Num a => Learner '[] a ()
delete = Learner
    { i        = \ParamsNull _   -> ()
    , u        = \ParamsNull _ _ -> ParamsNull
    , r        = \ParamsNull _ _ -> 0
    , iniParam = ParamsNull
    }

leftUnit :: Learner '[] ((), a) a
leftUnit = Learner
    { i        = \ParamsNull ((), a) -> a
    , u        = \ParamsNull _ _     -> ParamsNull
    , r        = \ParamsNull _ g     -> ((), g)
    , iniParam = ParamsNull
    }

rightUnit :: Learner '[] (a, ()) a
rightUnit = Learner
    { i        = \ParamsNull (a, ()) -> a
    , u        = \ParamsNull _ _     -> ParamsNull
    , r        = \ParamsNull _ g     -> (g, ())
    , iniParam = ParamsNull
    }

assoc :: Learner '[] ((a, b), c) (a, (b, c))
assoc = Learner
    { i        = \ParamsNull ((a, b), c)    -> (a, (b, c))
    , u        = \ParamsNull _ _            -> ParamsNull
    , r        = \ParamsNull _ (ga,(gb,gc)) -> ((ga, gb), gc)
    , iniParam = ParamsNull
    }