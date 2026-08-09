module Training.Training
    ( step
    , train
    , debug
    , stepMulti
    , trainMulti
    , accuracy
    , accuracyMulti
    ) where

import Core.Learner
import Core.Params
import Debug.Trace        (traceShow)
import Core.Multi         (Multi(..))
import Core.MultiLearner  (MultiLearner(..))

-- Learner
step :: Learner ps Double Double -> Params ps -> (Double, Double) -> Params ps
step model params (x, y) = u model params x y

train :: Learner ps Double Double -> Params ps -> [(Double, Double)] -> Int -> Params ps
train _     params _     0 = params
train model params pairs n =
    let params' = foldl (step model) params pairs
    in train model params' pairs (n - 1)

debug :: ShowParams ps => Learner ps Double Double -> Params ps -> [(Double, Double)] -> Int -> Params ps
debug _     params _     0 = params
debug model params pairs n =
    let params' = foldl (step model) params pairs
    in traceShow params' (debug model params' pairs (n - 1))

accuracy :: Learner ps Double Double -> Params ps -> [(Double, Double)] -> Double
accuracy _ _ [] = 0.0
accuracy model ps pairs =
    let acertos = length (filter correto pairs)

        correto (x, y) =
            let pred   = i model ps x
                classe = if pred >= 0.5 then 1.0 else 0.0
            in classe == y

    in fromIntegral acertos / fromIntegral (length pairs)


-- MultiLearner
stepMulti :: MultiLearner ps as b -> Params ps -> (Multi as, b) -> Params ps
stepMulti model params (xs, y) =
    uM model params xs y

trainMulti :: MultiLearner ps as b -> Params ps -> [(Multi as, b)] -> Int -> Params ps
trainMulti _     params _     0 = params
trainMulti model params pairs n =
    let params' = foldl (stepMulti model) params pairs
    in trainMulti model params' pairs (n - 1)

accuracyMulti :: MultiLearner ps '[[Double]] Double -> Params ps -> [(Multi '[[Double]], Double)] -> Double
accuracyMulti _ _ [] = 0.0
accuracyMulti model ps entries =
    let acertos = length (filter correto entries)
        correto (entrada, y) =
            let pred   = iM model ps entrada
                classe = if pred >= 0.5 then 1.0 else 0.0
            in classe == y
    in fromIntegral acertos / fromIntegral (length entries)