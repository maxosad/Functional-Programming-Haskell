{-# LANGUAGE InstanceSigs        #-}
module HW1.T7 
  ( ListPlus(..)
  , DotString(..)
  , Inclusive(..)
  , Fun(..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a deriving Show
infixr 5 :+
-- a:+ b :+ c :+ Last d
instance Semigroup (ListPlus a) where
  (<>) :: ListPlus a -> ListPlus a -> ListPlus a
  (<>) (Last a) x = a :+ x
  (<>) (a :+ y) x  = a :+ (y <> x)

{-
v1 = 3 :+ (Last 4)

v2 = 5 :+ 1 :+ (Last 2)
-}

data Inclusive a b = This a | That b | Both a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) :: Inclusive a b -> Inclusive a b -> Inclusive a b
  (<>) (Both x y) (Both x1 y1) = (Both ((<>) x x1) ((<>) y y1)) 
  (<>) (Both x y) (This x1) = (Both ((<>) x x1) y) 
  (<>) (Both x y) (That y1) = (Both x ((<>) y y1))  
  (<>) (This x) (Both x1 y1) = (Both ((<>) x x1) y1)  
  (<>) (This x) (This x1) = (This ((<>) x x1))  
  (<>) (This x) (That y1) = (Both x y1)  
  (<>) (That y) (Both x1 y1) = (Both x1 ((<>) y y1))  
  (<>) (That y) (This x1) = (Both x1 y)  
  (<>) (That y) (That y1) = (That ((<>) y y1))  
  


newtype DotString = DS String deriving Show

instance Monoid DotString where
  mempty :: DotString
  mempty = (DS "")
  
  mappend :: DotString -> DotString -> DotString
  mappend (DS "") x = x
  mappend x (DS "") = x
  mappend (DS a) (DS b) = (DS (a ++ "." ++  b))

instance Semigroup DotString where
  (<>) :: DotString -> DotString -> DotString
  (<>) = mappend


  
  
newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) :: Fun a -> Fun a -> Fun a
  (<>) (F f) (F f1) = (F (f1 . f))

instance Monoid (Fun a) where
  mempty :: Fun a
  mempty = (F id)
  
  mappend :: Fun a -> Fun a -> Fun a
  mappend = (<>)
