module HW2.T4
  ( State(..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , Prim(..)
  , eval
  , Expr(..)
  ) where

import HW2.T1
import Control.Monad

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S {runS = \s -> mapAnnotated f ((runS state) s)}
  

wrapState :: a -> State s a
wrapState a = S { runS = \s -> (a :# s)}


modifyState :: (s -> s) -> State s ()
modifyState f = S {runS = \s -> () :# (f s) }

joinState :: State s (State s a) -> State s a
joinState state = S{ runS = \s -> helper ((runS state) s) }

helper :: Annotated s (State s a) -> Annotated s a
helper (state :# s) = (runS state) s

geta :: Annotated e a -> a
geta (a :# e) = a

gete :: Annotated e a -> e
gete (a :# e) = e


getv :: State [Prim Double] Double -> Double
getv state = geta ((runS state) []) 



instance Functor (State s) where
  fmap = mapState
  
instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q
  
instance Monad (State s) where
  m >>= f = joinState (fmap f m)
  
  
data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum
  deriving Show

data Expr = Val Double | Op (Prim Expr) deriving Show

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs y = Op (Abs y)
  signum y = Op (Sgn y)
  fromInteger x = Val (fromInteger x)
  
instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)


eval :: Expr -> State [Prim Double] Double
eval (Val a) = pure a
eval (Op (Add a b)) = do 
  ea <- eval a
  eb <- eval b
  modifyState ((Add ea eb) :)
  return (ea + eb)
eval (Op (Sub a b)) = do 
  ea <- eval a
  eb <- eval b
  modifyState ((Sub ea eb) :)
  return (ea - eb)
eval (Op (Mul a b)) = do 
  ea <- eval a
  eb <- eval b
  modifyState ((Mul ea eb) :)
  return (ea * eb)
eval (Op (Div a b)) = do 
  ea <- eval a
  eb <- eval b
  modifyState ((Div ea eb) :)
  return (ea / eb)
eval (Op (Abs a)) = do
  ea <- eval a
  modifyState ((Abs ea) :)
  return (abs ea)
eval (Op (Sgn a)) = do
  ea <- eval a
  modifyState ((Sgn ea) :)
  return (signum ea)

{-
r1 = eval (-5)
r0 = runS (eval (1 + 2 + 3 + 4)) []
e0 = runS (eval (2 + 3 * 5 - 7)) []
e = runS (eval (abs(5) )) []
ee = runS (eval (abs(abs(5)) )) []
eee = runS (eval (abs(abs(abs(-5))) )) []
e1 = runS (eval (3 + 2 )) []
e2 = runS (eval (signum(3) )) []
var = (3.14 + 1.618 :: Expr)
val = (Val (-0.1))

s1 :: State [Prim Double] Double
s1 = S{runS = \s -> 1.0:#s}
s2 :: State [Prim Double] Double
s2 = S{runS = \s -> 2.0:#s}
s4 :: State [Prim Double] Double
s4 = S{runS = \s -> -3.0:#s}

s3 = liftM2 (+) s1 s2

s5 = liftM (abs) s4

q = runS p [(Mul 3 5)]
p = mapState (\() -> (-3.0)) (modifyState ff) 

ff :: [Prim Double] -> [Prim Double]
ff s = s ++ [(Mul 3 5)]

w = runS (mapState (abs) s4) []
-}