module Data.List.Util (
  hasSubset
, disjoint
, replaceByFst
, nubOn
, noDuplicates
) where


import Data.Function (on)
import Data.List ((\\), deleteBy, intersect, nub, nubBy)


hasSubset :: Eq a => [a] -> [a] -> Bool
hasSubset x y = null $ y \\ x


disjoint :: Eq a => [a] -> [a] -> Bool
disjoint x y = null $ x `intersect` y


replaceByFst :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
replaceByFst x = (x :) . deleteBy ((==) `on` fst) x


nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = nubBy ((==) `on` f)


noDuplicates :: Eq a => [a] -> Bool
noDuplicates x = length x == length (nub x)
