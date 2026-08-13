{-# LANGUAGE KindSignatures #-}

module Core.Multi (Multi(..)) where

import Data.Kind (Type)

infixr 5 :-:
data Multi (as :: [Type]) where
    MultiNull  :: Multi '[]
    (:-:)      :: a -> Multi as -> Multi (a ': as)