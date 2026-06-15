-- definindo a estrutura arvore
data Arvore a where
  Galho :: a -> (Arvore a) -> (Arvore a) -> Arvore a
  Folha :: a -> Arvore a
  Nulo  :: Arvore a
  deriving Show

-- Arvore(Integer)
arvore :: Arvore Integer
arvore = Galho 2 (Galho 7 (Folha 2)(Galho 6 (Folha 5)(Folha 11)))(Galho 5 Nulo (Galho 9 (Folha 4) Nulo))

-- instruindo o mapeamento de uma arvore
instance Functor Arvore where
  fmap :: (a -> b) -> Arvore a -> Arvore b
  fmap f (Galho x esq dir) = Galho (f x) (fmap f esq) (fmap f dir)
  fmap f (Folha x) = Folha (f x)
  fmap f Nulo = Nulo 

-- f(Arvore)
f :: Arvore Integer -> Arvore Integer
f = fmap (*2)

-- p(Arvore)
p :: Arvore Integer -> Arvore Bool
p = fmap even

-- mapeamento: Arvore(Integer) -> Lista(Integer) (pre-ordem)
listar :: Arvore Integer -> [Integer]
listar (Galho x esq dir) =  [x] ++ listar esq ++ listar dir
listar (Folha x) = [x]
listar Nulo = []

-- mapeamento: Arvore(Integer) -> Lista(Bool) (pre-ordem)
listarPares :: Arvore Integer -> [Bool] 
listarPares (Galho x esq dir) = [even x] ++ listarPares esq ++ listarPares dir
listarPares (Folha x) = [even x]
listarPares Nulo = []
