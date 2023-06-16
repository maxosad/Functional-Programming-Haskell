{-# LANGUAGE TypeOperators        #-}
module HW0.T1 -- <-> (..)
  ( type (<->) (..)
  , flipIso
  , runIso
  , distrib
  , assocPair
  , assocEither  
  ) where
import Data.Either
data a <-> b = Iso (a -> b) (b -> a)


flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a) = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)
             
--Iso ((a, (b, c)) -> ((a, b), c)) (((a, b), c) -> (a, (b, c)))
--assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = (Iso f1 f2)

f1 :: (a, (b, c)) -> ((a, b), c)
f1 (a, (b, c)) = ((a, b), c)

f2 :: ((a, b), c) -> (a, (b, c))
f2 ((a, b), c) = (a, (b, c))


--Iso (Either a (Either b c) -> Either (Either a b) c) (Either (Either a b) c -> Either a (Either b c))
--assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = (Iso e1 e2)

e1          :: Either a (Either b c) -> Either (Either a b) c
e1 (Left a) = (Left (Left a))
e1 (Right (Left b)) = (Left (Right b))
e1 (Right (Right c)) = (Right c)

e2          :: Either (Either a b) c -> Either a (Either b c)
e2 (Right c) = (Right (Right c))
e2 (Left (Left a)) = (Left a)
e2 (Left (Right b)) = (Right (Left b))

