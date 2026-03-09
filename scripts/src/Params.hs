module Params (Params(..), dimParams, projectFirstOnes, projectOthers, unify) where

-- extensões
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

import Data.Kind (Type)
import Data.List (intercalate)  

-- concatenação de type families
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
    '[] ++ ys       = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

-- espaço de paremetro: elemento neutro e concatenação
data Params (ps :: [Type]) where
    ParamsNull :: Params '[]
    (:::)      :: p -> Params ps -> Params (p ': ps)
    deriving stock (Eq)

-- define a precedência de avaliação de (:::)
infixr 5 :::

-- instância Show em Params manual para facilitar debug
class ShowParams ps where
    showParams :: Params ps -> [String]

instance ShowParams '[] where
    showParams ParamsNull = []

instance (Show p, ShowParams ps) => ShowParams (p ': ps) where
    showParams (x ::: xs) = show x : showParams xs

instance ShowParams ps => Show (Params ps) where
    show xs = "[" ++ intercalate ", " (showParams xs) ++ "]"


-- funções auxiliares para manipulação de parâmetros
-- dimensão do espaço de parâmetros
dimParams :: Params ps -> Int
dimParams ParamsNull = 0
dimParams (_ ::: xs) = 1 + dimParams xs

-- projeção dos primeiros n parâmetros
projectFirstOnes :: Int -> Params ps -> Params ps
projectFirstOnes 0 _ = ParamsNull
projectFirstOnes _ ParamsNull = ParamsNull
projectFirstOnes n (x ::: xs) = x ::: projectFirstOnes (n-1) xs

-- projeção dos demais parâmetros
projectOthers :: Int -> Params ps -> Params ps
projectOthers 0 xs = xs
projectOthers _ ParamsNull = ParamsNull
projectOthers n (_ ::: xs) = projectOthers (n-1) xs

-- unir duas listas de parâmetros
unify :: Params ps -> Params qs -> Params (ps ++ qs)
unify ParamsNull ys = ys
unify (x ::: xs) ys = x ::: unify xs ys