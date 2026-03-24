{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (id, (.))
import Unsafe.Coerce (unsafeCoerce)
import Cat
import Learner
import Params

{-
testes empíricos das leis categoriais de Learner

1. identidade à esquerda  (id . f  = f)
2. identidade à direita   (f . id  = f)
3. associatividade        (h.(g.f) = (h.g).f)

nota: unsafeCoerce é usado para forçar igualdades de tipos que são verdadeiras
por construção matemática, mas que o GHC não consegue provar automaticamente,
devido à não-injetividade da type family (++). ver relatório adjunto.
-}

-- f : '[Int] -> Int -> Int
f :: Learner '[Int] Int Int
f =
    Learner
    {
        i        = \(x ::: ParamsNull) a -> a + x,
        u        = \(x ::: ParamsNull) _ _ -> (x + 1) ::: ParamsNull,
        r        = \_ a _ -> a,
        iniParam = 2 ::: ParamsNull
    }

-- g : '[Int] -> Int -> Int
g :: Learner '[Int] Int Int
g =
    Learner
    {
        i        = \(x ::: ParamsNull) a -> a * x,
        u        = \(x ::: ParamsNull) _ _ -> (x + 1) ::: ParamsNull,
        r        = \_ a _ -> a,
        iniParam = 3 ::: ParamsNull
    }

-- h : '[Int] -> Int -> Int
h :: Learner '[Int] Int Int
h =
    Learner
    {
        i        = \(x ::: ParamsNull) a -> a - x,
        u        = \(x ::: ParamsNull) _ _ -> (x - 1) ::: ParamsNull,
        r        = \_ a _ -> a,
        iniParam = 4 ::: ParamsNull
    }

-- teste 1: identidade à esquerda (id . f = f)
testidl :: (Eq a, Eq b, Eq (Params ps))
        => Learner ps a b -> a -> b -> Bool
testidl lf a b =
    let left  = unsafeCoerce (id . lf) :: Learner ps a b
        right = lf
        pl    = iniParam left
        pr    = iniParam right
    in i left pl a    == i right pr a
    && u left pl a b  == u right pr a b
    && r left pl a b  == r right pr a b

-- teste 2: identidade à direita (f . id = f)
testidr :: (Eq a, Eq b, Eq (Params ps))
        => Learner ps a b -> a -> b -> Bool
testidr lf a b =
    let left  = unsafeCoerce (lf . id) :: Learner ps a b
        right = lf
        pl    = iniParam left
        pr    = iniParam right
    in i left pl a    == i right pr a
    && u left pl a b  == u right pr a b
    && r left pl a b  == r right pr a b

-- teste 3: associatividade (h.(g.f) = (h.g).f)
-- unsafeCoerce: (h.g).f :: Learner ((ps ++ qs) ++ rs) a d
--               h.(g.f) :: Learner (ps ++ (qs ++ rs)) a d
testassoc :: (Eq a, Eq d, Eq (Params ((ps ++ qs) ++ rs))) => Learner ps a b -> Learner qs b c -> Learner rs c d -> a -> d -> Bool
testassoc lf lg lh a d =
    let left  = unsafeCoerce  ((lh . lg) . lf) :: Learner ((ps ++ qs) ++ rs) a d
        right = unsafeCoerce  (lh . (lg . lf)) :: Learner ((ps ++ qs) ++ rs) a d
        pl    = iniParam left
        pr    = iniParam right
    in i left pl a    == i right pr a
    && u left pl a d  == u right pr a d
    && r left pl a d  == r right pr a d

main :: IO ()
main = do
    let x = 10 :: Int
    putStrLn $ "identidade a esquerda, entrada 10:  " ++ show (testidl f x x)
    putStrLn $ "identidade a direita,  entrada 10:  " ++ show (testidr f x x)
    putStrLn $ "associatividade,       entrada 10:  " ++ show (testassoc f g h x x)
