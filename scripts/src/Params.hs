{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}

module Params (Params(..), ShowParams(..), type (++), projectFirst, projectRest, unify) where

import Prelude           hiding ((++))
import qualified Prelude as P
import Data.Kind          (Type)
import Data.List          (intercalate)
import Unsafe.Coerce      (unsafeCoerce)
import GHC.Exts           (Any)

-- concatenação de listas no nível de tipos
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
    '[]       ++ ys  = ys
    (x ': xs) ++ ys  = x ': (xs ++ ys)


-- espaço de parâmetros: elemento neutro e concatenação (monóide livre)
data Params (ps :: [Type]) where
    ParamsNull :: Params '[]
    (:::)      :: p -> Params ps -> Params (p ': ps)

-- precedência de (:::), mesma de (:)
infixr 5 :::

-- talvez seja melhor tirar, ja que nao vamos fazer comparação...
-- instâncias Eq
deriving stock instance Eq (Params '[])
deriving stock instance (Eq p, Eq (Params ps)) => Eq (Params (p ': ps))

-- instância Show
class ShowParams ps where
    showParams :: Params ps -> [String]

instance ShowParams '[] where
    showParams ParamsNull = []

instance (Show p, ShowParams ps) => ShowParams (p ': ps) where
    showParams (x ::: xs) = show x : showParams xs

instance ShowParams ps => Show (Params ps) where
    show xs = "[" P.++ intercalate ", " (showParams xs) P.++ "]"
{-
obs.:
fica como a fazer, apóes a publicação do primeiro artigo, uma função fromParams para conversão Params -> Double
talvez algo que aproveite a instancia Show e faça algo como Params -> String -> Double
-}


-- projeção dos parâmetros do learner interno (retorna ps, ignora qs)
projectFirst ::  Params ps -> Params qs -> Params (ps ++ qs) -> Params ps
projectFirst ParamsNull    _  _    = ParamsNull
projectFirst (_ ::: rest) qs pqs  =
    case unsafeCoerce pqs :: Params Any of
        ParamsNull -> unsafeCoerce ParamsNull
        (x ::: xs) -> unsafeCoerce x ::: projectFirst rest qs (unsafeCoerce xs)

-- projeção dos parâmetros do learner externo (descarta ps, retorna qs)
projectRest :: Params ps -> Params qs -> Params (ps ++ qs) -> Params qs
projectRest ParamsNull    _  qs   = qs
projectRest (_ ::: rest) qs pqs  =
    case unsafeCoerce pqs :: Params Any of
        ParamsNull -> unsafeCoerce ParamsNull
        (_ ::: xs) -> projectRest rest qs (unsafeCoerce xs)

-- concatenação de dois espaços de parâmetros
unify :: Params ps -> Params qs -> Params (ps ++ qs)
unify ParamsNull  ys = ys
unify (x ::: xs) ys = x ::: unify xs ys