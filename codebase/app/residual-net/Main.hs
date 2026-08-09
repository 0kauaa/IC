module Main where

import Prelude hiding           (id, (.))
import Core.Cat                 (Cat(..))
import Core.Learner             (Learner(..), (//))
import Core.Params              (Params(..))
import Core.Utils               (mean, stddev)
import Sandbox.Layers           (denseLayer)
import Sandbox.Activations      (relu)
import Sandbox.Outputs          (mseOutput)
import Sandbox.Preprocessing    (zScore)
import Sandbox.Routing          (monoid, comonoid)
import Training.Training        (train, accuracy)
import Dataset.Synthetic.Linear (data_2_1)
import Unsafe.Coerce            (unsafeCoerce)

-- x ──── δ ────┬──── f ────┐
--              └──── id ───┴──── μ ──── f(x) + x
residualBlock :: Learner ps Double Double -> Learner ps Double Double
residualBlock f = unsafeCoerce $ monoid . (f // id) . comonoid

residualNet :: Double -> Double -> Learner '[Double, Double, Double, Double] Double Double
residualNet mu sigma = mseOutput . residualBlock (relu . denseLayer 0.5 0.0) . residualBlock (relu . denseLayer 0.3 0.0) . zScore mu sigma

main :: IO ()
main = do
    let mu    = mean   (map fst data_2_1)
        sigma = stddev (map fst data_2_1)
        model = residualNet mu sigma
        p0    = iniParam model
        ps    = train model p0 data_2_1 500
    
    putStrLn $ "parametros: " ++ show ps
    putStrLn $ "predicao para 17: " ++ show (i model ps 17)