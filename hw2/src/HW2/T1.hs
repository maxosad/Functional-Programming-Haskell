module HW2.T1
  ( Option(..)
  , Pair(..)
  , Quad(..)
  , Annotated(..)
  , Except(..)
  , Prioritised(..)
  , Stream(..)
  , List(..)
  , Fun(..)
  , Tree(..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree) where
  
data Option a = None | Some a deriving Show
data Pair a = P a a deriving Show
data Quad a = Q a a a a deriving Show
data Annotated e a = a :# e deriving Show
infix 0 :#
data Except e a = Error e | Success a deriving Show
data Prioritised a = Low a | Medium a | High a deriving Show
data Stream a = a :> Stream a deriving Show
infixr 5 :>
data List a = Nil | a :. List a deriving Show
infixr 5 :.
data Fun i a = F (i -> a)
data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving Show

mapOption      :: (a -> b) -> (Option a -> Option b) 
mapOption f (None) = (None)
mapOption f (Some a) = (Some (f a))

mapPair        :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P x y) = (P (f x) (f y)) 

mapQuad        :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a b c d) = (Q (f a) (f b) (f c) (f d))

mapAnnotated   :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = (f a) :# e

mapExcept      :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Success a) = (Success (f a))
mapExcept f (Error e) = (Error e)


mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a) = (Low (f a))
mapPrioritised f (Medium a) = (Medium (f a))
mapPrioritised f (High a) = (High (f a))


mapStream      :: (a -> b) -> (Stream a -> Stream b)
mapStream f (a :> b) = ((f a) :> (mapStream f b))


mapList        :: (a -> b) -> (List a -> List b)
mapList f (Nil) = (Nil)
mapList f (a :. b) = ((f a) :. (mapList f b))


mapFun         :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F f1) = (F (\x -> f (f1 x)))


mapTree        :: (a -> b) -> (Tree a -> Tree b)
mapTree f Leaf = Leaf
mapTree f (Branch l a r) = (Branch (mapTree f l) (f a) (mapTree f r)) 
