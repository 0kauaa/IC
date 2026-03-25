module LinearRegressor where

import Learner
import Params

-- learner regressor = r(p, x, y), onde p = {w, b}
regressor :: Learner '[Double, Double] Double Double -- Learner [w, b] x y
regressor = Learner
    {
        -- implementa uma reta
        i = \(w ::: b ::: ParamsNull) x ->
            x * w + b,

        -- dado um erro, atualiza os parâmetros
        u = \(w ::: b ::: ParamsNull) x y ->

                -- erro da prdição
            let e  = (x * w + b) - y          -- yhat - y
                -- novo w
                w' = w - ep * e * x           -- gradiente em w
                -- novo b
                b' = b - ep * e               -- gradiente em b
                
                -- novo espaço de parâmetros
                in w' ::: b' ::: Params Null,

        -- dado um erro, atualiza a entrada
        r = \(w ::: b ::: ParamsNull) x y ->
            
                -- erro da predição
            let e = (x * w + b) - y
                
                in a - ep * e * w,
        
        iniParam = 0.0 ::: 0.0 ::: ParamsNull

    }
    where ep = 0.1