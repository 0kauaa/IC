module Main where

import Prelude hiding (id, (.))
import Cat
import Learner

{-
testes estruturais da leis categoriais

1. identidade à esquerda (f.id    = f)
2. identidade à direita  (id.f    = f)
3. associatividade       (h.(g.f) = (h.g).f)
-}

-- f : Int -> Int
f :: Learner Int Int Int
f = 
    Learner
    {
        i = \p a   -> a + p, -- implementa a soma
        u = \p _ _ -> p + 1, -- atualiza o parâmetro
        r = \_ a _ -> a,     -- atualiza a entrada
        iniParam = 2
    }

-- g : Int -> Int
g :: Learner Int Int Int
g =
    Learner
    {
        i = \p a   -> a * p, -- implementa a multiplicação
        u = \p _ _ -> p + 1, -- atualiza o parâmetro
        r = \_ a _ -> a,     -- atualiza a entrada
        iniParam = 3
    }

-- h : Int -> Int
h :: Learner Int Int Int
h =
    Learner
    {
        i = \p a   -> a - p, -- implementa a subtração
        u = \p _ _ -> p + 1, -- atualiza o parâmetro
        r = \_ a _ -> a,     -- atualiza a entrada
        iniParam = 4
    }



-- teste 1: identidade à esquerda (id.f = f)
testidl :: Int -> Bool
testidl a =
    let comp  = id . f
        pcomp = iniParam comp
        p     = iniParam f 
    in i comp pcomp a == i f p a -- testa: i(id.f(p, a)) == i(f(p, a))



-- teste 2: identidade à direita (f.id = f)
testidr :: Int -> Bool
testidr a =
    let comp  = f . id
        pcomp = iniParam comp
        p     = iniParam f
    in i comp pcomp a == i f p a -- testa: i(f.id(p, a)) == i(f(p, a))



-- teste 3: associatividade (h.(g.f) = (h.g).f)
testassoc :: Int -> Bool
testassoc a =
    let left  = (h . g) . f
        right =  h . (g . f)
        
        pl    = iniParam left
        pr    = iniParam right
    
    in i left pl a == i right pr a -- testa: i(h.(g.f)(p, a) == i((h.g).f(p, a)))

main :: IO ()
main = do
    print ("Teste de composicao a esquerda, com entrada 10: ", testidl 10)
    print ("Teste de composicao a direita, com entrada 10: ", testidr 10)
    print ("Teste da associatividade da composicao para o morfismo I: ", testassoc 10)

{-
testassoc :: Int -> Int -> Bool
testassoc a1 a2 =
    let left  =  (h . g) . f
        right =  h . (g . f)

        pl    = iniParam left
        pr    = iniParam right

        i_l   = i left  pl a1
        i_r   = i right pr a1

    in
        i_l == i_r                           -- testa: i_l == i_r
    &&  u left pl a1 a2 == u right pr a1 a2  -- testa: u_l == u_r
    &&  r left pl a1 a2 == r right pr a1 a2  -- testa: r_l == r_r

-}