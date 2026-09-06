module Sandbox.Cat.Layers
    ( denseLayer
    ) where

import Core.Params
import Core.Learner

denseLayer :: Double -> Double -> Learner '[Double, Double] Double Double
denseLayer w0 b0 = Learner
    { i = \(w :|: b :|: ParamsNull) x   -> w * x + b
    , u = \(w :|: b :|: ParamsNull) x g -> (w - ep * g * x) :|: (b - ep * g) :|: ParamsNull
    , r = \(w :|: _ :|: ParamsNull) _ g -> g * w
    , iniParam = w0 :|: b0 :|: ParamsNull
    } where ep = 0.01