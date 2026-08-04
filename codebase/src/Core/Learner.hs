{-# LANGUAGE KindSignatures #-}

module Core.Learner
    ( Learner(..)
    , (//)
    ) where

import Prelude hiding (id, (.))
import Data.Kind      (Type)
import Core.Cat       (Cat(..))
import Core.Params    (Params(..), projectFirst, projectRest, unify, type (++))

data Learner (ps :: [Type]) a b = Learner
    { i        :: Params ps -> a -> b                -- implement
    , u        :: Params ps -> a -> b -> Params ps   -- update
    , r        :: Params ps -> a -> b -> a           -- request
    , iniParam :: Params ps                          -- parâmetro inicial
    }

instance Cat Learner where
    -- id de Learner
    id = Learner
        { i        = \ParamsNull a   -> a
        , u        = \ParamsNull _ _ -> ParamsNull
        , r        = \ParamsNull a _ -> a
        , iniParam = ParamsNull
        }

    -- composição sequencial (.)
    (.) (Learner i'' u'' r'' params'') (Learner i' u' r' params') = Learner
        { i = \params a ->
            let p = projectFirst params' params'' params
                q = projectRest  params' params'' params
                b = i' p a
            in i'' q b
        
        , u = \params a c ->
            let p     = projectFirst params' params'' params
                q     = projectRest  params' params'' params
                b     = i'  p a
                q'    = u'' q b c
                b_req = r'' q b c
                p'    = u'  p a b_req
            in unify p' q'

        , r = \params a c ->
            let p     = projectFirst params' params'' params
                q     = projectRest  params' params'' params
                b     = i'  p a
                b_req = r'' q b c
            in r' p a b_req

        , iniParam = unify params' params''
        }

-- composição paralela (//)
infixr 3 //
(//) :: Learner ps a b -> Learner qs c d -> Learner (ps ++ qs) (a, c) (b, d)
(//) (Learner i' u' r' params') (Learner i'' u'' r'' params'') = Learner
    { i = \params (a, c) ->
        let p = projectFirst params' params'' params
            q = projectRest  params' params'' params
        in (i' p a, i'' q c)

    , u = \params (a, c) (b, d) ->
        let p  = projectFirst params' params'' params
            q  = projectRest  params' params'' params
            p' = u' p a b
            q' = u'' q c d
        in unify p' q'
    
    , r = \params (a, c) (b, d) ->
        let p     = projectFirst params' params'' params
            q     = projectRest  params' params'' params
            a_req = r' p a b
            c_req = r'' q c d
        in (a_req, c_req)
    
    , iniParam = unify params' params''
    }