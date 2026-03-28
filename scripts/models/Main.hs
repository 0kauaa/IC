module Main where

r :: Double -> Double
r x = x * 2.7 + 1.9 

data :: [(Double, Double)]
data = [(x, r x) | x <- [1..60]]

main :: IO ()
main = putStrLn "..."
