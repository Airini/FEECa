module Discrete where

import Data.Type.Natural
import Data.List ((\\),nub)

-- Specific operations on naturals

factorial :: Nat -> Nat
factorial n = n * factorial (n - 1)

quotient :: Nat -> Nat -> Nat
quotient Z _ = Z
quotient (S x) y = S (quotient (S x - y) y)

combinations :: Nat -> Nat -> Nat
combinations n k = quotient (factorial n) ((factorial k) * (factorial (n - k)))


-- Sign of a permutation defined by a pair of increasing permutations
sign :: ([Int], [Int]) -> Int
sign (p1, p2) = sum [ length (filter (i <) p1) | i <- p2]


-- Picks elements at given indices
pick :: [Int] -> [a] -> [a]
pick [] xs         = []
pick (1:is) (x:xs) = x : pick is xs
pick (i:is) xs     = pick (i-1:is) xs

-- Returns differences between consecutive elements
differences :: [Int] -> [Int]
differences = fst . foldl (\(l,acc) x -> (l ++ [x-acc],x)) ([],0)

-- All possible increasing k-permutations of n elements
permutations :: Int -> Int -> [[Int]]
permutations n 0 = []
permutations n 1 = [ [j]  | j <- [1..n]]
permutations n k = [ j:xs | j <- [1..n], xs <- permutations n (k-1), head xs > j ]

-- All possible pairs of increasing k,j-permutations of n elements
permutationPairs :: Int -> Int -> Int -> [([Int],[Int])]
permutationPairs n k j =
    nub $ concatMap (\(f,ss) ->
                      map (\ixs -> (f,pick ixs ss)) (map (differences) secondix))
          pairedUp
  where firstperm = permutations n k
        secondix  = permutations (n-k) j
        secondels = map ([1..n] \\) firstperm
        pairedUp  = zip firstperm secondels

