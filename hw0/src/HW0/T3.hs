module HW0.T3 
  (  s
  ,  k
  ,  i
  ,  compose
  ,  contract
  ,  permute
  )  where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> a
k x y = x

i :: a -> a
i = s k k

sd :: b -> a -> a
sd = k i

seFth :: a -> b -> c -> b
seFth = k k

compose :: (b -> c) -> (a -> b) -> a -> c
compose = s (k s) k

contract :: (a -> a -> b) -> a -> b
contract = s s sd

permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (compose compose s) seFth