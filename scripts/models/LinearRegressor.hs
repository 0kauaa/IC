module LinearRegressor where

import Learner
import Params

-- learner regressor linear = rl([p], x, y), onde [p] = [{w, b}]
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
                e  = ŷ - y
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
                in x - ep * e * w,            -- gradiente em x
        
        iniParam = 0.0 ::: 0.0 ::: ParamsNull

    }
    where ep = 0.1

-- desce um passo no gradiente
step :: Learner ps Double Double
     -> Params ps           -- recebe os parametros atuais
     -> (Double, Double)    -- recebe um para de dados (x, y)
     -> Params ps           -- retorna novos parâmetros

step model params (x, y) = u modelo params x y

-- desce o gradiente em n passos
train :: Learner ps Double Double
      -> Params ps
      -> [(Double, Double)]
      -> Params ps

train _     params _    0 = params
trian model params data n =
    let params' = foldl (stap modelo) params data
    in train model params' dados (n - 1)