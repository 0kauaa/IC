{-# LANGUAGE KindSignatures, FlexibleInstances, UndecidableInstances, ScopedTypeVariables  #-}

module Core.PROPsLearner where

import Data.Kind     (Type)
import Core.Multi    (Multi(..))
import Core.Params   (Params(..), type (++))
import Core.PROPs    (PROPs(..))
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts      (Any)
import Core.Utils    (projectFirst, projectRest, multiLen, multiHead, multiTail, splitMulti, appendMulti, unify)


data PROPsLearner (ps :: [Type]) (as :: [Type]) (bs :: [Type]) = PROPsLearner
    { iP :: Params ps -> Multi as -> Multi bs
    , uP :: Params ps -> Multi as -> Multi bs -> Params ps
    , rP :: Params ps -> Multi as -> Multi bs -> Multi as
    , iniParamsP :: Params ps
    }

instance PROPs PROPsLearner where
    id  = PROPsLearner
        { iP         = \ParamsNull as   -> as
        , uP         = \ParamsNull _ _  -> ParamsNull
        , rP         = \ParamsNull _ gs -> gs
        , iniParamsP =  ParamsNull

        }

    (.) g f = PROPsLearner
        { iP = \params entry ->
                let ps  = projectFirst (iniParamsP f) (iniParamsP g) params
                    qs  = projectRest  (iniParamsP f) (iniParamsP g) params
                    mid = iP f ps entry
                in  iP g qs mid

        , uP = \params entry feedback ->
                let ps     = projectFirst (iniParamsP f) (iniParamsP g) params
                    qs     = projectRest  (iniParamsP f) (iniParamsP g) params
                    mid    = iP f ps entry
                    midReq = rP g qs mid feedback
                    qs'    = uP g qs mid feedback
                    ps'    = uP f ps entry midReq
                in  unify ps' qs'

        , rP = \params entry feedback ->
                let ps     = projectFirst (iniParamsP f) (iniParamsP g) params
                    qs     = projectRest  (iniParamsP f) (iniParamsP g) params
                    mid    = iP f ps entry
                    midReq = rP g qs mid feedback
                in  rP f ps entry midReq

        , iniParamsP = unify (iniParamsP f) (iniParamsP g)
        }

    (//) f g = PROPsLearner
        { iP = \params entry ->
                let ps               = projectFirst (iniParamsP f) (iniParamsP g) params
                    qs               = projectRest  (iniParamsP f) (iniParamsP g) params
                    (fEntry, gEntry) = splitMulti (multiLen (iniParamsP f)) entry
                    fOut             = iP f ps fEntry
                    gOut             = iP g qs gEntry
                in  appendMulti fOut gOut

        , uP = \params entry feedback ->
                let ps               = projectFirst (iniParamsP f) (iniParamsP g) params
                    qs               = projectRest  (iniParamsP f) (iniParamsP g) params
                    (fEntry, gEntry) = splitMulti (multiLen (iniParamsP f)) entry
                    (fFeed,  gFeed)  = splitMulti (multiLen (iniParamsP f)) feedback
                    ps'              = uP f ps fEntry fFeed
                    qs'              = uP g qs gEntry gFeed
                in  unify ps' qs'

        , rP = \params entry feedback ->
                let ps               = projectFirst (iniParamsP f) (iniParamsP g) params
                    qs               = projectRest  (iniParamsP f) (iniParamsP g) params
                    (fEntry, gEntry) = splitMulti (multiLen (iniParamsP f)) entry
                    (fFeed,  gFeed)  = splitMulti (multiLen (iniParamsP f)) feedback
                    fGrad            = rP f ps fEntry fFeed
                    gGrad            = rP g qs gEntry gFeed
                in  appendMulti fGrad gGrad

        , iniParamsP = unify (iniParamsP f) (iniParamsP g)
        }