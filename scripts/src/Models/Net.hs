module Models.Net (smallNet) where

import Prelude hiding (id, (.))
import Core.Cat       (Cat(..))
import Core.Learner   (Learner(..))
import Core.Params    (Params(..))


relu :: Learner '[] Double Double
relu = Learner
    {
        i = \ParamsNull  x -> 
            max 0 x,
        
        u = \ParamsNull _ _ ->
            ParamsNull,
        
        r = \ParamsNull x b_req ->
            if x > 0 then b_req else 0,

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

denseLayer :: Double -> Double -> Learner '[Double, Double] Double Double
denseLayer w0 b0 = Learner
    { 
        i = \(w ::: b ::: ParamsNull) x -> 
            w * x + b,

        u = \(w ::: b ::: ParamsNull) x y ->
            let e  = w * x + b - y
                w' = w - ep * e * x
                b' = b - ep * e
            in w' ::: b' ::: ParamsNull,

        r = \(w ::: b ::: ParamsNull) x y ->
            let e = w * x + b - y
            in x - ep * e * w,
            
        iniParam = w0 ::: b0 ::: ParamsNull
    }
  where ep = 0.01

layer1 = denseLayer 0.5 0.0
layer2 = denseLayer 0.3 0.0

smallNet :: Learner '[Double, Double, Double, Double] Double Double
smallNet = sigmoid . layer2 . relu . layer1