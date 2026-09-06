{-# LANGUAGE AllowAmbiguousTypes, DerivingStrategies, FlexibleInstances, FlexibleContexts, UndecidableInstances, RankNTypes, ScopedTypeVariables #-}

module Core.Params
    ( Params(..)
    , ShowParams(..)
    , type (++)
    ) where

import Prelude           hiding ((++))
import qualified Prelude as P
import Data.Kind          (Type)
import Data.List          (intercalate)
import Unsafe.Coerce      (unsafeCoerce)
import GHC.Exts           (Any)

-- GADT do espaço de parâmtros
infixr 5 :|:
data Params (ps :: [Type]) where
    ParamsNull  :: Params '[]
    (:|:)       :: p -> Params ps -> Params (p ': ps)

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
    showParams (x :|: xs) = show x : showParams xs

instance ShowParams ps => Show (Params ps) where
    show xs = "[" P.++ intercalate ", " (showParams xs) P.++ "]"