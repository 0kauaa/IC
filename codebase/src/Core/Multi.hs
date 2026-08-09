{-# LANGUAGE KindSignatures #-}

module Core.Multi (Multi(..)) where

import Data.Kind (Type)

data Multi (as :: [Type]) where
    MultiNull  :: Multi '[]
    (::::)     :: a -> Multi as -> Multi (a ': as)

infixr 5 ::::