--- 
module Stack(
    Stack (..),
    push, pop, top,
    empty, isEmpty
) where

data Stack a = Stk [a]
    deriving (Show)

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack empty"

top :: Stack a -> a
top (Stk (x:xs)) = x
top _ = error "Stack empty"

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty _ = False 
