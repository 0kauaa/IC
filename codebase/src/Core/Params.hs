{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Core.Params
    ( Params(..)
    , ShowParams(..)
    , type (++)
    , projectFirst
    , projectRest
    , unify 
    ) where

import Prelude           hiding ((++))
import qualified Prelude as P
import Data.Kind          (Type)
import Data.List          (intercalate)
import Unsafe.Coerce      (unsafeCoerce)
import GHC.Exts           (Any)

-- GADT do espaço de parâmtros
infixr 5 :::
data Params (ps :: [Type]) where
    ParamsNull :: Params '[]
    (:::)      :: p -> Params ps -> Params (p ': ps)

-- type family de concatenação entre espaço de parâmetros
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
    '[]       ++ ys  = ys
    (x ': xs) ++ ys  = x ': (xs ++ ys)

-- instância Show
class ShowParams ps where
    showParams :: Params ps -> [String]

instance ShowParams '[] where
    showParams ParamsNull = []

instance (Show p, ShowParams ps) => ShowParams (p ': ps) where
    showParams (x ::: xs) = show x : showParams xs

instance ShowParams ps => Show (Params ps) where
    show xs = "[" P.++ intercalate ", " (showParams xs) P.++ "]"

-- funções auxiliares para manipulação do espaço de parâmetros
projectFirst ::  Params ps -> Params qs -> Params (ps ++ qs) -> Params ps
projectFirst ParamsNull    _  _    = ParamsNull
projectFirst (_ ::: rest) qs pqs  =
    case unsafeCoerce pqs :: Params Any of
        ParamsNull -> unsafeCoerce ParamsNull
        (x ::: xs) -> unsafeCoerce x ::: projectFirst rest qs (unsafeCoerce xs)

projectRest :: Params ps -> Params qs -> Params (ps ++ qs) -> Params qs
projectRest ParamsNull    _  qs   = qs
projectRest (_ ::: rest) qs pqs  =
    case unsafeCoerce pqs :: Params Any of
        ParamsNull -> unsafeCoerce ParamsNull
        (_ ::: xs) -> projectRest rest qs (unsafeCoerce xs)

unify :: Params ps -> Params qs -> Params (ps ++ qs)
unify ParamsNull  ys = ys
unify (x ::: xs) ys = x ::: unify xs ys