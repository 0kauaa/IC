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

-- f : Params Int -> Int
f :: Learner '[Int] Int Int
f = 
    Learner
    {
        i = \p a   -> let (x ::: ParamsNull) = p in  a + x,                 -- implementa a soma
        u = \p _ _ -> let (x ::: ParamsNull) = p in (x + 1) ::: ParamsNull, -- atualiza o parâmetro
        r = \_ a _ -> a,                                                    -- atualiza a entrada
        iniParam = 2 ::: ParamsNull
    }

-- g : Params Int -> Int
g :: Learner Int Int Int
g =
    Learner
    {
        i = \p a   -> let (x ::: ParamsNull) = p in  a * x,                 -- implementa a multiplicação
        u = \p _ _ -> let (x ::: ParamsNull) = p in (x + 1) ::: ParamsNull, -- atualiza o parâmetro
        r = \_ a _ -> a,                                                    -- atualiza a entrada
        iniParam = 3 ::: ParamsNull
    }

-- h : Params Int -> Int
h :: Learner Int Int Int
h =
    Learner
    {
        i = \p a   -> let (x ::: ParamsNull) = p in  a - p,                 -- implementa a subtração
        u = \p _ _ -> let (x ::: ParamsNull) = p in (p - 1) ::: ParamsNull, -- atualiza o parâmetro
        r = \_ a _ -> a,                                                    -- atualiza a entrada
        iniParam = 4 ::: ParamsNull
    }



-- teste 1: identidade à esquerda (id.f = f)
testidl :: Eq b => Learner ps a b -> a -> b -> Bool
testidl f ps a b =
    let left  = id . f
        right = f

        pl  = iniParam left
        pr  = iniParam right
        
    in i left pl a   == i right pr a                              -- testa: i(id.f(p, a)) == i(f(p, a))
    && u left pl a b == u right pr a b                            -- testa: u(id.f(p, a, b) == u(f(p, a, b) 
    && r left pl a b == r right pr a b                            -- testa: r(id.f(p, a, b) == r(f(p, a, b)


-- teste 2: identidade à direita (f.id = f)
testidr :: Eq b => Learner ps a b -> a -> b -> Bool
testidr f ps a b =
    let left  = f . id
        right = f

        pl  = iniParam left
        pr  = iniParam right
        
    in i left pl a   == i right pr a                              -- testa: i(f.id(p, a)) == i(f(p, a))
    && u left pl a b == u right pr a b                            -- testa: u(f.id(p, a, b) == u(f(p, a, b) 
    && r left pl a b == r right pr a b                            -- testa: r(f.id(p, a, b) == r(f(p, a, b) 


-- teste 3: associatividade (h.(g.f) = (h.g).f)
testassoc :: Eq d => Learner ps a b -> Learner qs b c -> Learner rs c d -> a -> d -> Bool
testassoc f g h params a =
    let left  = (h . g) . f
        right =  h . (g . f)
        
        pl    = iniParam left
        pr    = iniParam right
    
    in i left pl a == i right pr a                                -- testa: i(h.(g.f)(p, a) == i((h.g).f(p, a))
    && u left pl a (i left pl a) == u right pr a (i right pr a)   -- testa: u(h.(g.f)(p, a) == u((h.g).f)(p, a))
    && r left pl a (i left pl a) == r right pr a (i right pr a)   -- testa: r(h.(g.f)(p, a) == r((h.g).f)(p, a))

main :: IO ()
main = do
    let x = 10
    putStrLn $ "teste de identidade a esquerda, com entrada 10: " ++ show (testidl f x x)
    putStrLn $ "teste de identidade a direita, com entrada 10: " ++ show (testidr f x x)
    putStrLn $ "teste de associatividade, com entrada 10: " ++ show (testassoc f g h x)