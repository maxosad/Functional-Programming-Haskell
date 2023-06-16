module HW1.T5 
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty ((<|), NonEmpty(..))


splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn pivot = foldr func ([] :| [])
  where
    func x acc@(y :| ys)
      | x == pivot   = [] <| acc
      | otherwise = (x : y) :| ys

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (a :| []) = a
joinWith pivot list = 
  foldl1 (\x  y -> x ++ [pivot] ++ y) list