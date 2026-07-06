module Training.Accuracy
    ( accuracy
    ) where

import Core.Learner
import Core.Params

accuracy :: Learner ps Double Double -> Params ps -> [(Double, Double)] -> Double
accuracy model ps pairs =
    let acertos = length (filter correto pairs)
        correto (x, y) =
            let pred = i model ps x
                classe = if pred >= 0.5 then 1.0 else 0.0
            in classe == y
    in fromIntegral acertos / fromIntegral (length pairs)