module Training.Training (step, train, debug) where

import Core.Learner
import Core.Params
import Debug.Trace (traceShow)


-- desce um passo no gradiente
step :: Learner ps Double Double -> Params ps -> (Double, Double) -> Params ps
step model params (x, y) = u model params x y

-- desce o gradiente em n passos
train :: Learner ps Double Double -> Params ps -> [(Double, Double)] -> Int -> Params ps
train _     params _     0 = params
train model params pairs n =
    let params' = foldl (step model) params pairs
    in train model params' pairs (n - 1)

-- treina o modelo, mas printando os parâmetros de cada passo
debug :: (ShowParams ps) => Learner ps Double Double -> Params ps -> [(Double, Double)] -> Int -> Params ps
debug _     params _     0 = params
debug model params pairs n =
    let params' = foldl (step model) params pairs
    in traceShow params' (debug model params' pairs (n - 1))