module HW2.T3 
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where
  
import HW2.T1
--data Option a = None | Some a deriving Show
joinOption    :: Option (Option a) -> Option a
joinOption (Some (Some a)) = (Some a)
joinOption _ = None

joinExcept    :: Except e (Except e a) -> Except e a
joinExcept (Error e) = (Error e)
joinExcept (Success (Error e)) = (Error e)
joinExcept (Success (Success a)) = (Success a)


joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e) :# e1) = a :# (e1 <> e)

-- data List a = Nil | a :. List a
joinList      :: List (List a) -> List a
joinList Nil = Nil
joinList ((Nil) :. q) = (joinList q)
joinList ((a :. p) :. q) = a :. (joinList (p :. q))


joinFun       :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = (F (\x -> ( (helper (f x)) x)) ) 

helper :: Fun i a -> (i -> a) 
helper (F f) = f

{-
listConcat :: List a -> List a -> List a
listConcat Nil q = q
listConcat (a :. p) q = a :. (listConcat p q)

-}
e0 = joinList ( (1:.2:.3:.Nil) :. (4:.5:.Nil):.Nil )
e1 = joinList ( (Nil) :. (4:.5:.Nil):.Nil )
e2 = joinList ( Nil )

