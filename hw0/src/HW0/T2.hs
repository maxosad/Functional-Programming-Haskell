{-# LANGUAGE TypeOperators        #-}
module HW0.T2 
  ( Not(..)
  , doubleNeg
  , reduceTripleNeg
  )where
import Data.Void
type Not a = a -> Void


doubleNeg :: a -> Not (Not a)
doubleNeg a b = b a 

reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg a = (\x -> a (doubleNeg x))
