module HW2.T5 
  ( ExceptState(..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , EvaluationError(..)
  , eval
  ) where

import HW2.T1
import HW2.T2
import HW2.T4(Prim(..), Expr(..))
import Control.Monad

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f state = ES {runES = \s -> mapExcept (mapAnnotated f) ((runES state) s)}


wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES {runES = \s ->  wrapExcept (a :# s)}

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState state = ES {runES = \s -> helper ((runES state) s)}

helper :: Except e (Annotated s (ExceptState e s a)) -> Except e (Annotated s a)
helper (Error e) = (Error e)
helper (Success (a :# s)) = (runES a) s

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES {runES = \s -> wrapExcept (() :# (f s))}


throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES {runES = \s -> (Error e)}



instance Functor (ExceptState e s ) where
  fmap = mapExceptState
  

instance Applicative (ExceptState e s ) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q
  
instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)



data EvaluationError = DivideByZero deriving Show


divideByZero :: ExceptState EvaluationError [Prim Double] Double
divideByZero = throwExceptState DivideByZero 

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val a) = pure a
eval (Op (Add a b)) = do
  ea <- eval a
  eb <- eval b
  modifyExceptState ((Add ea eb) :)
  return (ea + eb)
eval (Op (Sub a b)) = do
  ea <- eval a
  eb <- eval b
  modifyExceptState ((Sub ea eb) :)
  return (ea - eb)
eval (Op (Mul a b)) = do
  ea <- eval a
  eb <- eval b
  modifyExceptState ((Mul ea eb) :)
  return (ea * eb)
eval (Op (Div a b)) = do
  ea <- eval a
  eb <- eval b
  modifyExceptState ((Div ea eb) :)
  case eb of 
    0 -> divideByZero
    _ -> return (ea /eb)
eval (Op (Abs a)) = do
  ea <- eval a
  modifyExceptState ((Abs ea) :)
  return (abs ea) 
eval (Op (Sgn a)) = do
  ea <- eval a
  modifyExceptState ((Sgn ea) :)
  return (signum ea)
{-

r0 = runES (eval (1 + 2 + 3 + 4)) []
e0 = runES (eval (2 + 3 * 5 - 7)) []

e = runES (eval (abs(5) )) []
ee = runES (eval (abs(abs(5)) )) []
eee = runES (eval (abs(abs(abs(5))) )) []

d0 = runES (eval (1/0) ) []
d1 = runES (eval ((1/0) / (1/0))  ) []
-}