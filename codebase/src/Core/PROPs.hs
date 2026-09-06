{-# LANGUAGE PolyKinds #-}

module Core.PROPs 
    ( PROPs(..)
    ) where

import Prelude hiding (id, (.))
import Data.Kind      (Type)
import Core.Params    (type (++))

class PROPs (cat :: [Type] -> [Type] -> [Type] -> Type) where
    id   :: cat '[] '[a] '[a]
    (.)  :: cat qs bs cs -> cat ps as bs -> cat (ps ++ qs) as cs
    (//) :: cat ps as bs -> cat qs cs ds -> cat (ps ++ qs) (as ++ cs) (bs ++ ds)