{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}

module Core.Learner (Learner(..)) where

import Prelude hiding (id, (.))
import Data.Kind      (Type)
import Core.Cat       (Cat(..))
import Core.Params    (Params(..), projectFirst, projectRest, unify)

-- morfismo parametrizado: Learner ps a b
data Learner (ps :: [Type]) a b = Learner
    {
        i        :: Params ps -> a -> b,               -- implement
        u        :: Params ps -> a -> b -> Params ps,  -- update
        r        :: Params ps -> a -> b -> a,          -- request
        iniParam :: Params ps                          -- parâmetro inicial
    }

instance Cat Learner where

    -- identidade: parâmetro vazio, funções identidade
    id = Learner
        {
            i        = \ParamsNull a   -> a,
            u        = \ParamsNull _ _ -> ParamsNull,
            r        = \ParamsNull a _ -> a,
            iniParam = ParamsNull
        }

    --  g :: Learner qs b c
    --  f :: Learner ps a b
    -- gf :: Learner (ps ++ qs) a c
    (.) (Learner i'' u'' r'' params'') (Learner i' u' r' params') = Learner
        {
            -- implement: aplica f, depois g
            i = \params a ->
                let p = projectFirst params' params'' params
                    q = projectRest  params' params'' params
                    b = i' p a
                in i'' q b,

            -- update: propaga gradiente de g para f
            u = \params a c ->
                let p     = projectFirst params' params'' params
                    q     = projectRest  params' params'' params
                    b     = i'  p a
                    q'    = u'' q b c
                    b_req = r'' q b c
                    p'    = u'  p a b_req
                in unify p' q',

            -- request: propaga pedido de entrada de g para f
            r = \params a c ->
                let p     = projectFirst params' params'' params
                    q     = projectRest  params' params'' params
                    b     = i'  p a
                    b_req = r'' q b c
                in r' p a b_req,

            iniParam = unify params' params''
        }
