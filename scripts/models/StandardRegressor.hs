{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module StandardRegressor (standardlizedRegressor, step, train, debug) where

import Prelude hiding (id, (.))
import Cat (Cat(..))
import Params
import Learner
import LinearRegressor (regressor)
import Debug.Trace (traceShow)

-- padronização zscore
standardlizer :: Double -> Double -> Learner '[] Double Double
standardlizer mu sigma = Learner
    {
        -- padronização
        i = \ParamsNull x   -> (x - mu) / sigma,
        -- não parametrizado 
        u = \ParamsNull _ _ -> ParamsNull,
        -- gradiente da entrada 
        r = \ParamsNull _ z -> z / sigma, -- dE/dx = z * 1/sigma = z/sigma
        iniParam = ParamsNull
    }

-- learner regressor normalizado = zrl(mu, sigma, [p], x, y)
standardlizedRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
standardlizedRegressor mu sigma = regressor . standardlizer mu sigma

-- desce um passo no gradiente (mesmo da regressão simples)
step :: Learner ps Double Double -> Params ps -> (Double, Double) -> Params ps
step model params (x, y) = u model params x y

-- desce o gradiente em n passos (mesmo da regressão simples)
train :: Learner ps Double Double -> Params ps -> [(Double, Double)] -> Int -> Params ps
train _     params _     0 = params
train model params pairs n =
    let params' = foldl (step model) params pairs
    in train model params' pairs (n - 1)

-- treina o modelo, mas printando os parâmetros de cada passo
debug :: ShowParams ps => Learner ps Double Double -> Params ps -> [(Double, Double)] -> Int -> Params ps
debug _     params _     0 = params
debug model params pairs n =
    let params' = foldl (step model) params pairs
    in traceShow params' (debug model params' pairs (n - 1))