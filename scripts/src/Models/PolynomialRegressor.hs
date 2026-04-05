module Models.PolynomialRegressor (polynomialRegressor) where

import Prelude hiding           ((.))
import Core.Cat                 ((.))
import Core.Params              (Params(..))
import Core.Learner
import Models.StandardRegressor (standardlizer)
import Models.LinearRegressor   (linearRegressor)

-- ajuste polinomial
polynomialAdjuster :: Learner '[] Double Double
polynomialAdjuster = Learner
    {
        -- ajuste quadratico
        i = \ParamsNull x   -> x * x,
        -- não parametrizado
        u = \ParamsNull _ _ -> ParamsNull,
        -- gradiente da entrada (regra da cadeia)
        r = \ParamsNull x y -> y * 2 * x,
        iniParam = ParamsNull
    }

polynomialRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
polynomialRegressor mu sigma = linearRegressor . standardlizer mu sigma . polynomialAdjuster