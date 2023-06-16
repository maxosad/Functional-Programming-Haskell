module HW1.T6 
  ( mcat
  , epart
  ) where

func :: Monoid a => (Maybe a) -> a -> a
func Nothing b = b
func (Just a) b = a <> b

mcat :: Monoid a => [Maybe a] -> a
mcat = foldr func mempty 

f :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
f (Left e) (x, y) = (e <> x, y)
f (Right e) (x, y) = (x, e <> y)
  
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr f (mempty, mempty) 
  


