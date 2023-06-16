module HW2.T2
  ( distOption
  , distPair
  , distQuad
  , distAnnotated
  , distExcept
  , distPrioritised
  , distStream
  , distList
  , distFun
  , wrapOption
  , wrapPair
  , wrapQuad
  , wrapAnnotated
  , wrapExcept
  , wrapPrioritised
  , wrapStream
  , wrapList
  , wrapFun
  ) where

import HW2.T1

distOption      :: (Option a, Option b) -> Option (a, b)
distOption (None, None) = None
distOption (None, (Some b)) = None
distOption ((Some a), None) = None
distOption ((Some a), (Some b)) = (Some (a,b))


distPair        :: (Pair a, Pair b) -> Pair (a, b)
distPair ((P a a1), (P b b1)) = (P (a, b) (a1, b1))


distQuad        :: (Quad a, Quad b) -> Quad (a, b)
distQuad ((Q a1 a2 a3 a4), (Q b1 b2 b3 b4)) = 
  (Q (a1, b1) (a2, b2) (a3, b3) (a4, b4))
  
  
distAnnotated   :: Semigroup e => (Annotated e a, Annotated e b) ->
  Annotated e (a, b)
distAnnotated ((a :# e), (b :# e1)) = (a, b) :# (e <> e1) 


distExcept      :: (Except e a, Except e b) -> Except e (a, b)
distExcept ((Error e), _) = (Error e)
distExcept  (_, (Error e)) = (Error e)
distExcept  ((Success a), (Success b)) = (Success (a, b))


distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised ((Low a), (Low b)) = (Low (a, b))
distPrioritised ((Low a), (Medium b)) = (Medium (a, b))
distPrioritised ((Low a), (High b)) = (High (a, b))
distPrioritised ((Medium a), (Low b)) = (Medium (a, b))
distPrioritised ((Medium a), (Medium b)) = (Medium (a, b))
distPrioritised ((Medium a), (High b)) = (High (a, b))
distPrioritised ((High a), (Low b)) = (High (a, b))
distPrioritised ((High a), (Medium b)) = (High (a, b))
distPrioritised ((High a), (High b)) = (High (a, b))



distStream      :: (Stream a, Stream b) -> Stream (a, b)
distStream ((a :> s), (b :> s1)) = 
  ((a, b) :> (distStream (s, s1)))


distList        :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (a :. l, p@(b :. l1)) = 
  listConcat (firstWithAll (a:.Nil, p)) (distList (l, p))

firstWithAll :: (List a, List b) -> List (a, b)
firstWithAll (Nil, _) = Nil
firstWithAll (_, Nil) = Nil
firstWithAll (q@(a :. _), p@(b :. l1)) = 
  (a,b) :. (firstWithAll (q, l1)) 

listConcat :: List a -> List a -> List a
listConcat Nil q = q
listConcat (a :. p) q = a :. (listConcat p q)

distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun ((F f), (F f1)) = (F (\x -> (f x, f1 x)))

{-
distList ((1 :. 2 :. 3 :. Nil) , (10 :. 11 :. 12 :. Nil))


listConcat (1 :. 2 :. 3 :. Nil) (10 :. 11 :. 12 :. Nil)

firstWithAll ((1 :. 2 :. 3 :. Nil) , (10 :. 11 :. 12 :. Nil))
-}



wrapOption      :: a -> Option a
wrapOption a = (Some a)

wrapPair        :: a -> Pair a
wrapPair a = (P a a)

wrapQuad        :: a -> Quad a
wrapQuad a = (Q a a a a)

wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept      :: a -> Except e a
wrapExcept a = (Success a)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised a = (Low a)


wrapStream      :: a -> Stream a
wrapStream a = a :> (wrapStream a)

wrapList        :: a -> List a
wrapList a = a :. Nil

wrapFun         :: a -> (Fun i a)
wrapFun a = (F (\x -> a))