module FEECa.Utility.Discrete where

--import Data.Type.Natural
import Data.List ((\\),nub)
import Debug.Trace

-- Specific operations on naturals

--factorial :: Nat -> Nat
factorial :: Int -> Int
factorial n = n * factorial (n - 1)

--quotient :: Nat -> Nat -> Nat
quotient :: Int -> Int -> Int
--quotient Z _ = Z
--quotient (S x) y = S (quotient (S x - y) y)
quotient = div

--combinations :: Nat -> Nat -> Nat
combinations :: Int -> Int -> Int
combinations n k = quotient (factorial n) (factorial k * factorial (n - k))


-- Sign of a permutation defined by a pair of increasing permutations
-- sign :: Field f => ([Int], [Int]) -> f
-- sign (p1, p2) = if (sum [ length (filter (i <) p1) | i <- p2]) `mod` 2 == 0 then mulId else addInv mulId


-- Picks elements at given indices
pick :: [Int] -> [a] -> [a]
-- TODO: missing case: error?? eller controlled above
pick [] xs         = []
pick (1:is) (x:xs) = x : pick is xs
pick (i:is) (x:xs) = pick (i-1:is) xs
--pick (i:is) xs | i<0 = error "mep"

-- Returns differences between consecutive elements
differences :: [Int] -> [Int]
differences = fst . foldl (\(l,acc) x -> (l ++ [x-acc],x)) ([],0)

-- All possible increasing k-permutations of n elements
permutations :: Int -> Int -> [[Int]]
permutations n 0 = [[]]
permutations n 1 = [ [j]  | j <- [1..n]]
permutations n k = [ j:xs | j <- [1..n], xs <- permutations n (k-1), head xs > j ]

-- All possible pairs of increasing k,j-permutations of n elements
permutationPairs :: Int -> Int -> Int -> [([Int],[Int])]
permutationPairs n k j =
    nub $ concatMap (\(f,ss) ->
                      map ((\ixs -> (f,pick ixs ss)) . differences) secondix)
          pairedUp
  where firstperm = permutations n k
        secondix  = permutations (n-k) j
        secondels = map ([1..n] \\) firstperm
        pairedUp  = zip firstperm secondels

-- All possible k-j pairs such that k+j <= n
arityPairs :: Int -> [(Int,Int)]
arityPairs n = [ (k, j) | k <- [1..n], j <- [1..(n-k)] ]
