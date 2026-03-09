module Learner (Learner(..)) where

import Cat (Cat(..))
import Params (Params(..), dimParams, projectFirstOnes, projectOthers, unify)


-- Arr(Learner)
data Learner (ps :: [Type]) a b = Learner
    {
        i :: Params ps -> a -> b,                  -- implement : dados p, a,    retorna b
        u :: Params ps -> a -> b -> Params ps,     -- update    : dados p, a, b, retorna b
        r :: Params ps -> a -> b -> a,             -- request   : dados p, a, b, retorna a
        iniParam :: Params ps                      -- valor inicial do espaço de parâmetros
    }

-- categoria Learner
instance Cat Learner where
    id = Learner
        {
            i = \ParamsNull a   -> a,           -- id(I(p, a))    = a
            u = \ParamsNull _ _ -> ParamsNull,  -- id(U(p, a, b)) = p
            r = \ParamsNull a _ -> a,           -- id(r(p, a, b)) = a
            iniParam = ParamsNull               -- id(P)          = p
        }

    --  g :: Learner qs b c
    --  f :: Learner ps a b
    -- gf :: Learner (ps ++ qs) a c
    (.) :: Learner qs b c -> Learner ps a b -> Learner (ps ++ qs) a c
    (Learner i'' u'' r'' params'') . (Learner i' u' r' params') = Learner
        {
            -- implement
            i = \params a ->
                let p = projectFirstOnes (dimParams params') params
                    q = projectOthers    (dimParams params') params

                    b = i' p a  -- saida de f
                in i'' q b,     -- g calculada com a saida de f

            -- update
            u = \params a c ->
                let p = projectFirstOnes (dimParams params') params
                    q = projectOthers    (dimParams params') params

                    b        = i'  p a
                    q'       = u'' q b c
                    b_req    = r'' q b c
                    p'       = u'  p a b_req
                in unify p' q',     -- parametros atualizados

            -- request
            r = \params a c ->
                let p = projectFirstOnes (dimParams params') params
                    q = projectOthers    (dimParams params') params

                    b        = i'  p a
                    b_req    = r'' q b c
                    a_req    = r'  p a b_req
                in a_req,           -- entrada atualizada

        iniParam = unify params' params'' --
        }
