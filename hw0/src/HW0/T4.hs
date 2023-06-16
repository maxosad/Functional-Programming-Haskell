module HW0.T4 
  ( repeat'
  , map'
  , fib
  , fac 
  ) where
  
import Data.Function 
import Numeric.Natural (Natural)


repeat' :: a -> [a]          
repeat' x = fix (x:)

  
map' f = fix (\acc xs -> case xs of
  [] -> []
  (x:xs') -> (f x) : acc xs')
  


fib :: Natural -> Natural       -- computes the n-th Fibonacci number
fib x = fst $ fibh x

fibh = fix (\acc x -> case x of 
  0 -> (0,0)
  1 -> (1,0)
  _ -> let (x1,x2) = acc(x-1) in (x2 + x1, x1) )



fac :: Natural -> Natural      
m0 = map' (\x -> x + 1) [1,2,3,4,5]
fac = fix (\acc x -> case x of  
  0 -> 1 
  _ -> x * acc (x - 1))

fi0 = fib 7
r0 = take 4 (repeat' 3)
fa0 = fac 4

test = do 
  putStr (show fi0 ++ "\n")
  putStr (show m0 ++ "\n")
  putStr (show r0 ++ "\n")
  putStr (show fa0 ++ "\n")