-- id_str
idStr :: String -> String
idStr texto = texto

-- id_int
idInt :: Int -> Int
idInt inteiro = inteiro

-- id_bool
idBool :: Bool -> Bool
idBool booleano = booleano

-- morfismo t: str -> int
t :: String -> Int
t = length

-- morfismo p: int -> bool
p :: Int -> Bool
p = even

-- composição p ∘ t: str -> bool
-- (.) :: (b -> c) -> (a -> b) -> a -> c
pt :: String -> Bool
pt = p . t

-- morfismo s: bool -> str
s :: Bool -> String
s bool =  if bool 
          then "o tamanho da palavra eh par"
          else "o tamanho da palavra eh impar"

-- associatividade da composição
spt :: String -> String
spt = s . p . t 

comp1 :: String -> String
comp1 = (s . p) . t

comp2 :: String -> String
comp2 = s . (p . t)


-- functor T: List(String) -> List(Int) 
functorT :: [String] -> [Int]
functorT = map t

-- functor P: List(Int) -> List(Bool)
functorP :: [Int] -> [Bool]
functorP = map p

-- functor PT: List(String) -> (Bool)
functorPT :: [String] -> [Bool]
functorPT = functorP . functorT