module Combinatorics(rank, unrank, unrankIndices, sublists) where

import Math.Combinatorics.Exact.Binomial
import Data.Maybe(fromJust)
import Data.List (find)


-- | Compute the index of an increasing list of integers
rank :: [Int] -> Int
rank = rankRec 1

rankRec :: Int -> [Int] -> Int
rankRec i [] = 0
rankRec i (l:ls) = (l-1) `choose` i + rankRec (i+1) ls

unrankIndices :: Int -> Int -> [Int]
unrankIndices k n = map (\x -> x - 1) (unrankRec [] k n)

-- | Compute the increasing list corresponding to a given index n and length k.
unrank :: Int -> Int -> [Int]
unrank = unrankRec []

unrankRec :: [Int] -> Int -> Int -> [Int]
unrankRec l 0 _ = l
unrankRec l k 0 = unrankRec (k:l) (k-1) 0
unrankRec l k n = unrankRec (c:l) (k-1) (n-((c - 1) `choose` k))
    where c = fromJust (find (\x -> n < (x `choose` k)) [k..])

-- | Ranks of the k-1 increasing sublists of the k-increasing list given by its rank n
sublists :: Int -> Int -> [Int]
sublists 1 _ = [0]
sublists k n = sublists' [] 0 k n

sublists' :: [Int] -> Int -> Int -> Int -> [Int]
sublists' l _ 0 _ = l
sublists' ls r k n = sublists' (n':ls) (r + ccc) (k-1) (n - cc)
    where c = fromJust (find (\x -> n < (x `choose` k)) [k..])
          cc = (c-1) `choose` k
          ccc = (c-1) `choose` (k-1)
          n' = r + n - cc
