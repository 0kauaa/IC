{-# LANGUAGE KindSignatures, FlexibleInstances, UndecidableInstances, ScopedTypeVariables  #-}

module Core.MultiLearner where

import Data.Kind     (Type)
import Core.Multi    (Multi(..))
import Core.Params   (Params(..), type (++))
import Core.MultiCat (MultiCat(..))
import Unsafe.Coerce (unsafeCoerce)
import Core.Utils    (projectFirst, projectRest, multiLen, multiHead, multiTail, splitMulti, appendMulti, unify)

data MultiLearner (ps :: [Type]) (as :: [Type]) b = MultiLearner
    { iM :: Params ps -> Multi as -> b
    , uM :: Params ps -> Multi as -> b -> Params ps
    , rM :: Params ps -> Multi as -> b -> Multi as
    , iniParamsM :: Params ps
    }

instance MultiCat MultiLearner where
    id = MultiLearner
        { iM         = \ParamsNull (a :-: MultiNull) -> a
        , uM         = \ParamsNull _ _               -> ParamsNull
        , rM         = \ParamsNull as _              -> as
        , iniParamsM =  ParamsNull
        }

    (.) g f = MultiLearner
        { iM = \params entry ->
            let ps       = projectFirst (iniParamsM f) (iniParamsM g) params
                qs       = projectRest  (iniParamsM f) (iniParamsM g) params
                (as, bs) = splitMulti   (multiLen (iniParamsM f)) entry
                b        = iM f ps as
            in  iM g qs (b :-: bs)
        , uM = \params entry c ->
            let ps       = projectFirst (iniParamsM f) (iniParamsM g) params
                qs       = projectRest  (iniParamsM f) (iniParamsM g) params
                (as, bs) = splitMulti   (multiLen (iniParamsM f)) entry
                b        = iM f ps as
                bReq     = multiHead (rM g qs (b :-: bs) c)
                qs'      = uM g qs (b :-: bs) c
                ps'      = uM f ps as bReq
            in unify ps' qs'
        , rM = \params entry c ->
            let ps       = projectFirst (iniParamsM f) (iniParamsM g) params
                qs       = projectRest  (iniParamsM f) (iniParamsM g) params
                (as, bs) = splitMulti   (multiLen (iniParamsM f)) entry
                b        = iM f ps as
                bReq     = multiHead (rM g qs (b :-: bs) c)
                fGrad    = rM f ps as bReq
                gGrad    = multiTail (rM g qs (b :-: bs) c)
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