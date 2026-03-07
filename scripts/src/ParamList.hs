-- extensões
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

import Data.Kind (Type)
import Data.List (intercalate)

-- lista de parâmetros heterogênea
data ParamList (ps :: [Type]) where
    ParamNull :: ParamList '[]
    (:::)     :: p -> ParamList ps -> ParamList (p ': ps) 

-- precedência de avaliação da concatenação de ParamList
infixr 5 :::

-- instanciando show para ParamList
class ShowParamList ps where
    showParams :: ParamList ps -> [String]

instance ShowParamList '[] where
    showParams ParamNull = []

instance (Show p, ShowParamList ps) => ShowParamList (p ': ps) where
    showParams (x ::: xs) = show x : showParams xs

instance ShowParamList ps => Show (ParamList ps) where
    show xs = "[" ++ intercalate ", " (showParams xs) ++ "]"