module Learner (Learner(..)) where

import Cat


-- Arr(Learner)
data Learner (ps :: [Type]) a b = Learner
    {
        i :: ParamList ps -> a -> b,       -- implement : dados p, a,    retorna b
        u :: ParamList ps -> a -> b -> p,  -- update    : dados p, a, b, retorna b
        r :: ParamList ps -> a -> b -> a,  -- request   : dados p, a, b, retorna a
        iniParam :: ParamList              -- valor inicial do espaço de parâmetros
    }

-- categoria Learner
instance Cat Learner where
    id = Learner
        {
            i = \() a -> a,     -- id(I(p, a)) = a
            u = \() _ _ -> (),  -- id(U(p, a, b)) = p
            r = \() a _ -> a,   -- id(r(p, a, b)) = a
            iniParam = ()       -- id(P) = p
        }

    --  g :: Learner q b c
    --  f :: Learner p a b
    -- gf :: Learner (p, q) a c
    (.) :: Learner q b c -> Learner p a b -> Learner (p, q) a c
    (Learner i'' u'' r'' q0) . (Learner i' u' r' p0) = Learner
        {
            -- implement
            i = \(p, q) a ->
                let b = i' p a  -- saida de g
                in i'' q b,     -- saida de f

            -- update
            u = \(p, q) a c ->
                let b       = i' p a
                    q'      = u'' q b c
                    b_req   = r'' q b c
                    p'      = u' p a b_req
                in (p', q'),    -- parametros atualizados

            -- request
            r = \(p, q) a c ->
                let b       = i' p a
                    b_req   = r'' q b c
                    a_req   = r' p a b_req
                in a_req,       -- entrada atualizada

        iniParam = (p0, q0)
        }
