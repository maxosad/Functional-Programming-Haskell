module HW1.T4 
  ( tfoldr
  ) where
  
--data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a)
import HW1.T3 

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ b Leaf = b
tfoldr f b (Branch _ l a r) = tfoldr f (f a (tfoldr f b r)) l
