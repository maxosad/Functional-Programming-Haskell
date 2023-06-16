module HW1.T3 
  ( Tree(..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where
  
data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) deriving Show

tsize :: Tree a -> Int
tsize Leaf = 0 
tsize (Branch (s, _) _ _ _) = s

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, d) _ _ _) = d

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember e (Branch _ l el r) = if el == e 
  then True
  else if e < el 
    then tmember e l
    else tmember e r


mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l e r = 
  (Branch (1 + (tsize l) + (tsize r), 1 + (max (tdepth l) (tdepth r))) l e r)


tinsert :: Ord a => a -> Tree a -> Tree a
tinsert e tree = if tmember e tree 
  then tree
  else (insert e tree)
  

insert :: Ord a => a -> Tree a -> Tree a
insert e Leaf = (Branch (1,1) Leaf e Leaf)  
insert e (Branch _ l el r) = if e < el 
  then rotate (mkBranch (insert e l) el r)
  else rotate (mkBranch l el (insert e r))




rotate :: Tree a -> Tree a
rotate Leaf  = Leaf 
rotate tree = let (t1, f1) = (smallLeft tree) in
  if f1 
    then t1
    else let (t2, f2) = (bigLeft tree) in
      if f2
        then t2
        else let (t3, f3) = (smallRight tree) in
          if f3
            then t3
            else let (t4, f4) = (bigRight tree) in
              if f4
                then t4
                else tree

smallLeft :: Tree a -> (Tree a, Bool)
smallLeft (Branch _ l a bb@(Branch _ c b r) ) = 
  if (((tdepth bb) - (tdepth l) == 2) && (tdepth c) <= (tdepth r))
    then (mkBranch (mkBranch l a c) b r, True)
    else (Leaf, False)
smallLeft _ = (Leaf, False)
  
  
bigLeft :: Tree a -> (Tree a, Bool)
bigLeft (Branch _ l a bb@(Branch _ cc@(Branch _ m c n) b r)) =
  if (((tdepth bb) - (tdepth l) == 2) && (tdepth cc) > (tdepth r))
    then (mkBranch (mkBranch l a m) c (mkBranch n b r), True)
    else (Leaf, False)
bigLeft _ = (Leaf, False)

smallRight :: Tree a -> (Tree a, Bool) 
smallRight (Branch _ bb@(Branch _ l b c) a r) =
  if ((tdepth bb) - (tdepth r) == 2 && (tdepth c) <= (tdepth l))
    then (mkBranch l b (mkBranch c a r), True)
    else (Leaf, False)
smallRight _ = (Leaf, False)

bigRight :: Tree a -> (Tree a, Bool)
bigRight (Branch _ bb@(Branch _ l b cc@(Branch _ m c n)) a r) =
  if ((tdepth bb) - (tdepth r) == 2 && (tdepth cc) > (tdepth l))
  then (mkBranch (mkBranch l b m) c (mkBranch n a r), True)
  else (Leaf, False)
bigRight _ = (Leaf, False)


tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf 
