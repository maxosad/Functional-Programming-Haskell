module HW0.T5 
  ( Nat(..)
  , nz
  , ns
  , nplus
  , nmult
  , nFromNatural
  , nToNum 
  ) where
import Numeric.Natural (Natural)
type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz f x = x

-- ((a -> a) -> a -> a) -> (a -> a) -> a -> a
ns :: Nat a -> Nat a 
ns f g x = f g (g x)


nplus :: Nat a -> Nat a -> Nat a
nplus f g h x = f h (g h x)
p = nplus (nFromNatural 4) (nFromNatural 6) inc 0

nmult :: Nat a -> Nat a -> Nat a
nmult f g h x = f (g $ h) x
m = nmult (nFromNatural 4) (nFromNatural 6) inc 0


nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural x = ns (nFromNatural (x-1))

inc :: Num a => a -> a
inc x = x + 1


nToNum :: Num a => Nat a -> a
nToNum f = f inc 0

n = nToNum $ ns $ ns $ ns $ nz
n1 = nFromNatural 2
n2 = nFromNatural 5
n3 = nplus n1 n2
n4 = nmult n1 n2

num3 = nToNum n3
num4 = nToNum n4

test = do 
  putStr (show n ++ "\n")    
  putStr (show num3 ++ "\n")    
  putStr (show num4 ++ "\n")    
