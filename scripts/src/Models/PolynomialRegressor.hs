module Models.PolynomialRegressor where

{-module Models.PolynomialRegressor (polynomialRegressor) where

import Prelude hiding         (id, (.))
import Core.Cat               (id, (.))
import Core.Params
import Core.Learner
import Models.LinearRegressor (linearRegressor)

-- ajuste polinomial
polynomialAdjuster :: Learner '[Double, Double] Double Double
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

polynomialRegressor :: Learner '[Double, Double] Double Double
polynomialRegressor = linearRegressor . polynomial
-}