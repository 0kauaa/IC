module Core.Utils 
    ( interpret
    , mean
    , stddev
    , multiHead
    , multiTail
    , multiLen
    , appendMulti
    , splitMulti
    , projectFirst
    , projectRest
    , unify
    ) where

import Prelude hiding ((.))
import Core.Cat       (Cat(..))
import Core.Params    (Params(..), type (++))
import Core.Multi     (Multi(..))
import GHC.Exts       (Any)
import Unsafe.Coerce  (unsafeCoerce)

-- remove parâmetros aprendidos do espaço normalizado
interpret :: Double -> Double -> Params '[Double, Double] -> Params '[Double, Double]
interpret mu sigma (w :|: b :|: ParamsNull) = 
    let w' = (w / sigma) 
        b' = b - (w * mu) / sigma
    in w' :|: b' :|: ParamsNull


-- funções estatísticas
mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

stddev :: [Double] -> Double
stddev [] = 0
stddev xs =
    let avg      = mean xs
        n        = fromIntegral (length xs)
        variance = sum (map (\x -> (x - avg) ^ (2 :: Int)) xs) / (n - 1)
    in sqrt variance


-- funções auxiliares para Multi
multiHead :: Multi (a ': as) -> a
multiHead (x :-: _) = x

multiTail :: Multi (a ': as) -> Multi as
multiTail (_ :-: xs) = xs

multiLen :: Params ps -> Int
multiLen ParamsNull  = 0
multiLen (_ :|: ps)  = 1 + multiLen ps

appendMulti :: Multi as -> Multi bs -> Multi (as ++ bs)
appendMulti MultiNull   ys = ys
appendMulti (x :-: xs)  ys = x :-: appendMulti xs ys

splitMulti :: Int -> Multi (as ++ bs) -> (Multi as, Multi bs)
splitMulti 0 xs = (unsafeCoerce MultiNull, unsafeCoerce xs)
splitMulti n xs =
    case (unsafeCoerce xs :: Multi (Any ': '[Any])) of
         (x :-: rest) ->
            let (as, bs) = splitMulti (n-1) (unsafeCoerce rest)
            in  (unsafeCoerce (x :-: as), bs)
  where _ = xs

-- auxiliar para Params
projectFirst ::  Params ps -> Params qs -> Params (ps ++ qs) -> Params ps
projectFirst ParamsNull    _  _    = ParamsNull
projectFirst (_ :|: rest) qs pqs  =
    case unsafeCoerce pqs :: Params Any of
        ParamsNull -> unsafeCoerce ParamsNull
        (x :|: xs) -> unsafeCoerce x :|: projectFirst rest qs (unsafeCoerce xs)

projectRest :: Params ps -> Params qs -> Params (ps ++ qs) -> Params qs
projectRest ParamsNull    _  qs   = qs
projectRest (_ :|: rest) qs pqs  =
    case unsafeCoerce pqs :: Params Any of
        ParamsNull -> unsafeCoerce ParamsNull
        (_ :|: xs) -> projectRest rest qs (unsafeCoerce xs)

unify :: Params ps -> Params qs -> Params (ps ++ qs)
unify ParamsNull  ys = ys
unify (x :|: xs) ys = x :|: unify xs ys
splitParams :: Params ps -> Params qs -> Params (ps ++ qs) -> (Params ps, Params qs)
splitParams ps qs params = (projectFirst ps qs params, projectRest ps qs params)