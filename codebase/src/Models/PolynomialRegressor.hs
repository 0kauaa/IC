module Models.PolynomialRegressor
    ( polynomialAdjuster
    , polynomialRegressor) where

import Prelude hiding           ((.))
import Core.Cat                 ((.))
import Core.Params              (Params(..))
import Core.Learner
import Models.StandardRegressor (standardlizer)
import Models.LinearRegressor   (linearRegressor)

-- ajuste polinomial
polynomialAdjuster :: Learner '[] Double Double
polynomialAdjuster = Learner
    { i = \ParamsNull x   -> x * x
    , u = \ParamsNull _ _ -> ParamsNull
    , r = \ParamsNull x y -> y * 2 * x
    , iniParam = ParamsNull
    }

polynomialRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
polynomialRegressor mu sigma = linearRegressor . standardlizer mu sigma . polynomialAdjuster