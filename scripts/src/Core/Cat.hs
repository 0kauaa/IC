{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds     #-}

module Cat (Cat(..)) where

import Prelude hiding (id, (.))
import Data.Kind      (Type)
import Params         (type (++))

class Cat (cat :: [Type] -> Type -> Type -> Type) where
    id  :: cat '[] a a
    (.) :: cat qs b c -> cat ps a b -> cat (ps ++ qs) a c