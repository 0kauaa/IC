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

main :: IO ()
main = do
    let x = 10 :: Int

    -- teste 1: identidade à esquerda (id . f = f)
    let idl    = unsafeCoerce (id . f) :: Learner '[Int] Int Int
        pl_idl = iniParam idl
        pr_f   = iniParam f
    putStrLn "--- identidade à esquerda (id . f = f) ---"
    putStrLn $ "  i (id.f) = " ++ show (i idl pl_idl x)
    putStrLn $ "  i  f     = " ++ show (i f   pr_f   x)
    putStrLn $ "  u (id.f) = " ++ show (u idl pl_idl x x)
    putStrLn $ "  u  f     = " ++ show (u f   pr_f   x x)
    putStrLn $ "  r (id.f) = " ++ show (r idl pl_idl x x)
    putStrLn $ "  r  f     = " ++ show (r f   pr_f   x x)

    -- teste 2: identidade à direita (f . id = f)
    let idr    = unsafeCoerce (f . id) :: Learner '[Int] Int Int
        pl_idr = iniParam idr
    putStrLn "--- identidade à direita (f . id = f) ---"
    putStrLn $ "  i (f.id) = " ++ show (i idr pl_idr x)
    putStrLn $ "  i  f     = " ++ show (i f   pr_f   x)
    putStrLn $ "  u (f.id) = " ++ show (u idr pl_idr x x)
    putStrLn $ "  u  f     = " ++ show (u f   pr_f   x x)
    putStrLn $ "  r (f.id) = " ++ show (r idr pl_idr x x)
    putStrLn $ "  r  f     = " ++ show (r f   pr_f   x x)

    -- teste 3: associatividade ((h.g).f = h.(g.f))
    let hgf_l  = unsafeCoerce ((h . g) . f) :: Learner '[Int, Int, Int] Int Int
        hgf_r  = unsafeCoerce (h . (g . f)) :: Learner '[Int, Int, Int] Int Int
        pl_l   = iniParam hgf_l
        pl_r   = iniParam hgf_r
    putStrLn "--- associatividade ((h.g).f = h.(g.f)) ---"
    putStrLn $ "  i (h.g).f = " ++ show (i hgf_l pl_l x)
    putStrLn $ "  i  h.(g.f) = " ++ show (i hgf_r pl_r x)
    putStrLn $ "  u (h.g).f = " ++ show (u hgf_l pl_l x x)
    putStrLn $ "  u  h.(g.f) = " ++ show (u hgf_r pl_r x x)
    putStrLn $ "  r (h.g).f = " ++ show (r hgf_l pl_l x x)
    putStrLn $ "  r  h.(g.f) = " ++ show (r hgf_r pl_r x x)