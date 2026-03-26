module LinearRegressor where

import Learner
import Params

-- learner regressor linear = r([p], x, y), onde [p] = [{w, b}]
regressor :: Learner '[Double, Double] Double Double -- Learner [w, b] x y
regressor = Learner
    {
        -- implementa uma reta
        i = \(w ::: b ::: ParamsNull) x ->
            x * w + b,

        -- dado um erro, atualiza os parâmetros
        u = \(w ::: b ::: ParamsNull) x y ->

                -- predição
            let ŷ  = (x * w + b)
                -- erro da prdição
                e  = ŷ - y                 -- yhat - y
                -- novo w
                w' = w - ep * e * x           -- gradiente em w
                -- novo b
                b' = b - ep * e               -- gradiente em b
                
                -- novo espaço de parâmetros
                in w' ::: b' ::: ParamsNull,

        -- dado um erro, atualiza a entrada
        r = \(w ::: b ::: ParamsNull) x y ->
            
                -- predição
            let ŷ = (x * w + b)
                -- erro da predição
                e = ŷ - y
                
                -- novas entradas
                in x - ep * e * w,
        
        iniParam = 0.0 ::: 0.0 ::: ParamsNull

    }
    where ep = 0.1