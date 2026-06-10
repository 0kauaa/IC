module Models.Net (smallNet) where

import Prelude hiding (id, (.))
import Core.Cat       (Cat(..))
import Core.Learner   (Learner(..))
import Core.Params    (Params(..))

zscore :: Double -> Double -> Learner '[] Double Double
zscore mu sigma = Learner
    {
        i = \ParamsNull x   -> 
            (x - mu) / sigma,

        u = \ParamsNull _ _ -> 
            ParamsNull,

        r = \ParamsNull _ z ->
            z / sigma,

        iniParam = ParamsNull
    }

denseLayer :: Double -> Double -> Learner '[Double, Double] Double Double
denseLayer w0 b0 = Learner
    { 
        i = \(w ::: b ::: ParamsNull) x -> 
            w * x + b,
        u = \(w ::: b ::: ParamsNull) x grad ->
            (w - ep * grad * x) ::: (b - ep * grad) ::: ParamsNull,

        r = \(w ::: _ ::: ParamsNull) _ grad ->
            grad * w,

        iniParam = w0 ::: b0 ::: ParamsNull
    } 
    where ep = 0.01

relu :: Learner '[] Double Double
relu = Learner
    {
        i = \ParamsNull  x -> 
            max 0 x,
        
        u = \ParamsNull _ _ ->
            ParamsNull,
        
        r = \ParamsNull x grad ->
            if x > 0 then grad else 0,

        iniParam = ParamsNull
    }

bceOutput :: Learner '[] Double Double
bceOutput = Learner
    {
        i = \ParamsNull z   ->
            1.0 / (1.0 + exp (-z)),

        u = \ParamsNull _ _ -> 
            ParamsNull,

        r = \ParamsNull z y ->
            let s = 1.0 / (1.0 + exp (-z))
            in  s - y,
        
        iniParam = ParamsNull
    }

sigmoid :: Learner '[] Double Double
sigmoid = Learner
    {
        i = \ParamsNull z -> 
            1.0 / (1.0 + exp (-z)),

        u = \ParamsNull _ _ -> 
            ParamsNull,
            
        r = \ParamsNull z y -> 
            let s = 1.0 / (1.0 + exp (-z))
            in  s - y,
        iniParam = ParamsNull
    }

-- mini rede
layer1 :: Learner [Double, Double] Double Double
layer1 = denseLayer 0.5 0.0

layer2 :: Learner [Double, Double] Double Double
layer2 = denseLayer 0.3 0.0

smallNet :: Double -> Double -> Learner '[Double, Double, Double, Double] Double Double
smallNet mu sigma = bceOutput . layer2 . relu . layer1 . zscore mu sigma