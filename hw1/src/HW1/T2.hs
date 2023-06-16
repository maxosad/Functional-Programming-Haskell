module HW1.T2 
  ( N(..)
  , nplus
  , nmult
  , nsub
  , ncmp
  , nFromNatural
  , nToNum
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where
  
import Numeric.Natural (Natural)
import Data.Maybe (isJust, fromJust)
data N = Z | S N deriving Show

nplus :: N -> N -> N 
nplus x Z = x
nplus Z x = x
nplus (S x) (S y) = S ( S ( nplus x y))

nmult :: N -> N -> N
nmult Z _ = Z
nmult _ Z = Z
nmult x (S y) = nplus x (nmult x y)

nsub :: N -> N -> Maybe N
nsub Z Z = Just Z
nsub Z _ = Nothing
nsub x Z = Just x
nsub (S x) (S y) = nsub x y 

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S x) (S y) = ncmp x y

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = (S (nFromNatural (x - 1)))

nToNum :: Num a => N -> a
nToNum Z = 0 
nToNum (S x) = 1 + nToNum x
 
nEven :: N -> Bool -- chet
nEven Z = True
nEven (S x) = nOdd x

nOdd :: N -> Bool -- neChet
nOdd Z = False
nOdd (S x) = nEven x

ndiv :: N -> N -> N 
ndiv _ Z = error "can't divide on 0"
ndiv x y = let tmp = (nsub x y) in
  if isJust tmp
  then (S (ndiv (fromJust tmp) y)) 
  else Z

nmod :: N -> N -> N 
nmod _ Z = error "can't divide on 0"
nmod x y = let tmp = (nsub x y) in
  if isJust tmp
  then nmod (fromJust tmp)  y
  else x



