module Cat (Cat(..)) where

class Cat cat where
    id  :: cat () a a
    (.) :: cat q b c -> cat p a b -> cat (p, q) a c