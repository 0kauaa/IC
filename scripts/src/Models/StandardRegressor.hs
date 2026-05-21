{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Models.StandardRegressor (standardlizer) where

import Prelude hiding         ((.))
import Core.Cat               ((.))
import Core.Params            (Params(..))
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
        r = \ParamsNull _ z -> z / sigma,    -- de/dx = z * 1/sigma = z/sigma
        iniParam = ParamsNull
    }

