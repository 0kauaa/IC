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
    { i :: Params ps -> Multi as -> b
    , u :: Params ps -> Multi as -> b -> Params ps
    , r :: Params ps -> Multi as -> b -> Multi as
    , iniParams :: Params ps
    }

instance MultiCat MultiLearner where
    id = MultiLearner
        { i        = \ParamsNull (a :::: MultiNull) -> a
        , u        = \ParamsNull _ _          -> ParamsNull
        , r        = \ParamsNull as _         -> as
        , iniParams =  ParamsNull
        }

    (.) g f = MultiLearner
        { i = \params entry ->
            let ps       = projectFirst (iniParams f) (iniParams g) params
                qs       = projectRest  (iniParams f) (iniParams g) params
                (as, bs) = splitMulti   (multiLen (iniParams f)) entry
                b        = i f ps as
            in  i g qs (b :::: bs)
        , u = \params entry c ->
            let ps       = projectFirst (iniParams f) (iniParams g) params
                qs       = projectRest  (iniParams f) (iniParams g) params
                (as, bs) = splitMulti   (multiLen (iniParams f)) entry
                b        = i f ps as
                bReq     = multiHead (r g qs (b :::: bs) c)
                qs'      = u g qs (b :::: bs) c
                ps'      = u f ps as bReq
            in unify ps' qs'
        , r = \params entry c ->
            let ps       = projectFirst (iniParams f) (iniParams g) params
                qs       = projectRest  (iniParams f) (iniParams g) params
                (as, bs) = splitMulti   (multiLen (iniParams f)) entry
                b        = i f ps as
                bReq     = multiHead (r g qs (b :::: bs) c)
                fGrad    = r f ps as bReq
                gGrad    = multiTail (r g qs (b :::: bs) c)
            in appendMulti fGrad gGrad
        , iniParams = unify (iniParams f) (iniParams g)
        }
 
    (//) f g = MultiLearner
        { i = \params entry ->
            let ps       = projectFirst (iniParams f) (iniParams g) params
                qs       = projectRest  (iniParams f) (iniParams g) params
                (as, bs) = splitMulti   (multiLen (iniParams f)) entry
            in (i f ps as, i g qs bs)
        , u = \params entry (bf, bg) ->
            let ps       = projectFirst (iniParams f) (iniParams g) params
                qs       = projectRest  (iniParams f) (iniParams g) params
                (as, bs) = splitMulti   (multiLen (iniParams f)) entry
                ps'      = u f ps as bf
                qs'      = u g qs bs bg
            in unify ps' qs'
        , r = \params entry (bf, bg) ->
            let ps       = projectFirst (iniParams f) (iniParams g) params
                qs       = projectRest  (iniParams f) (iniParams g) params
                (as, bs) = splitMulti   (multiLen (iniParams f)) entry
                fGrad    = r f ps as bf
                gGrad    = r g qs bs bg
            in appendMulti fGrad gGrad
        , iniParams = unify (iniParams f) (iniParams g)
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
