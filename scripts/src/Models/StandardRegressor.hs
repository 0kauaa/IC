{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Models.StandardRegressor (standardlizedRegressor) where

import Prelude hiding         (id, (.))
import Core.Cat               (Cat(..))
import Core.Params
import Core.Learner
import Models.LinearRegressor (linearRegressor)

-- padronização zscore
standardlizer :: Double -> Double -> Learner '[] Double Double
standardlizer mu sigma = Learner
    {
        -- padronização
        i = \ParamsNull x   -> (x - mu) / sigma,
        -- sem peso 
        u = \ParamsNull _ _ -> ParamsNull,
        -- gradiente da entrada 
        r = \ParamsNull _ z -> z / sigma, -- dE/dx = z * 1/sigma = z/sigma
        iniParam = ParamsNull
    }

-- learner regressor normalizado = rl(p, z(x), z(y)), com z(x) = (x - mu) / sigma
standardlizedRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
standardlizedRegressor mu sigma = linearRegressor . standardlizer mu sigma