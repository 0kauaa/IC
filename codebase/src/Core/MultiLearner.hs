{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Core.MultiLearner where

import Data.Kind     (Type)
import Core.Multi    (Multi(..))
import Core.Params   (Params(..), type (++), projectFirst, projectRest, unify)
import Core.MultiCat (MultiCat(..))
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts      (Any)

data MultiLearner (ps :: [Type]) (as :: [Type]) b = MultiLearner
    { iM :: Params ps -> Multi as -> b
    , uM :: Params ps -> Multi as -> b -> Params ps
    , rM :: Params ps -> Multi as -> b -> Multi as
    , iniParamsM :: Params ps
    }

instance MultiCat MultiLearner where
    id = MultiLearner
        { iM         = \ParamsNull (a :::: MultiNull) -> a
        , uM         = \ParamsNull _ _          -> ParamsNull
        , rM         = \ParamsNull as _         -> as
        , iniParamsM =  ParamsNull
        }

    (.) g f = MultiLearner
        { iM = \params entry ->
            let ps       = projectFirst (iniParamsM f) (iniParamsM g) params
                qs       = projectRest  (iniParamsM f) (iniParamsM g) params
                (as, bs) = splitMulti   (multiLen (iniParamsM f)) entry
                b        = iM f ps as
            in  iM g qs (b :::: bs)
        , uM = \params entry c ->
            let ps       = projectFirst (iniParamsM f) (iniParamsM g) params
                qs       = projectRest  (iniParamsM f) (iniParamsM g) params
                (as, bs) = splitMulti   (multiLen (iniParamsM f)) entry
                b        = iM f ps as
                bReq     = multiHead (rM g qs (b :::: bs) c)
                qs'      = uM g qs (b :::: bs) c
                ps'      = uM f ps as bReq
            in unify ps' qs'
        , rM = \params entry c ->
            let ps       = projectFirst (iniParamsM f) (iniParamsM g) params
                qs       = projectRest  (iniParamsM f) (iniParamsM g) params
                (as, bs) = splitMulti   (multiLen (iniParamsM f)) entry
                b        = iM f ps as
                bReq     = multiHead (rM g qs (b :::: bs) c)
                fGrad    = rM f ps as bReq
                gGrad    = multiTail (rM g qs (b :::: bs) c)
            in appendMulti fGrad gGrad
        , iniParamsM = unify (iniParamsM f) (iniParamsM g)
        }
 
    (//) f g = MultiLearner
        { iM = \params entry ->
            let ps       = projectFirst (iniParamsM f) (iniParamsM g) params
                qs       = projectRest  (iniParamsM f) (iniParamsM g) params
                (as, bs) = splitMulti   (multiLen (iniParamsM f)) entry
            in (iM f ps as, iM g qs bs)
        , uM = \params entry (bf, bg) ->
            let ps       = projectFirst (iniParamsM f) (iniParamsM g) params
                qs       = projectRest  (iniParamsM f) (iniParamsM g) params
                (as, bs) = splitMulti   (multiLen (iniParamsM f)) entry
                ps'      = uM f ps as bf
                qs'      = uM g qs bs bg
            in unify ps' qs'
        , rM = \params entry (bf, bg) ->
            let ps       = projectFirst (iniParamsM f) (iniParamsM g) params
                qs       = projectRest  (iniParamsM f) (iniParamsM g) params
                (as, bs) = splitMulti   (multiLen (iniParamsM f)) entry
                fGrad    = rM f ps as bf
                gGrad    = rM g qs bs bg
            in appendMulti fGrad gGrad
        , iniParamsM = unify (iniParamsM f) (iniParamsM g)
        }
 

-- funções auxiliares para Multi
multiHead :: Multi (a ': as) -> a
multiHead (x :::: _) = x

multiTail :: Multi (a ': as) -> Multi as
multiTail (_ :::: xs) = xs

multiLen :: Params ps -> Int
multiLen ParamsNull  = 0
multiLen (_ ::: ps)  = 1 + multiLen ps

appendMulti :: Multi as -> Multi bs -> Multi (as ++ bs)
appendMulti MultiNull   ys = ys
appendMulti (x :::: xs)  ys = x :::: appendMulti xs ys

splitMulti :: Int -> Multi (as ++ bs) -> (Multi as, Multi bs)
splitMulti 0 xs = (unsafeCoerce MultiNull, unsafeCoerce xs)
splitMulti n xs =
    case (unsafeCoerce xs :: Multi (Any ': '[Any])) of
         (x :::: rest) ->
            let (as, bs) = splitMulti (n-1) (unsafeCoerce rest)
            in  (unsafeCoerce (x :::: as), bs)
  where _ = xs

-- auxiliar para Params
splitParams :: Params ps -> Params qs -> Params (ps ++ qs) -> (Params ps, Params qs)
splitParams ps qs params = (projectFirst ps qs params, projectRest ps qs params)
