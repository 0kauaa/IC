{-# LANGUAGE PolyKinds #-}

module Core.MultiCat 
    ( MultiCat(..)
    ) where

import Prelude hiding (id, (.))
import Data.Kind      (Type)
import Core.Params    (type (++))

class MultiCat (cat :: [Type] -> [Type] -> Type -> Type) where
    id   :: cat '[] '[a] a
    (.)  :: cat qs (b ': bs) c -> cat ps as b -> cat (ps ++ qs) (as ++ bs) c
    (//) :: cat ps as b -> cat qs bs d -> cat (ps ++ qs) (as ++ bs) (b, d)

