-- monomorfismo
primeiroInt :: [Int] -> Maybe Int
primeiroInt [] = Nothing
primeiroInt (x:_) = Just x

-- polimorfismo paramétrico
primeiro :: [a] -> Maybe a
primeiro [] = Nothing
primeiro (a:_) = Just a

-- morfismos numéricos
somarInt :: [Int] -> Int 
somarInt = sum

somarFloat :: [Float] -> Float
somarFloat = sum

-- polimorfismo ad-hoc
somar :: Num a => [a] -> a
somar = sum