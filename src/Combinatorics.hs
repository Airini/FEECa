module Combinatorics(rank,
                     unrank,
                     unrankIndices,
                     sublists,
                     sumRLists) where

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
unrank :: Integral a => a -> a -> [a]
unrank k n = unrankRec [] k n

unrankRec :: Integral a => [a] -> a -> a -> [a]
unrankRec l 0 _ = l
unrankRec l k 0 = unrankRec (k:l) (k-1) 0
unrankRec l k n = unrankRec (c:l) (k-1) (n-((c - 1) `choose` k))
    where c = fromJust (find (\x -> n < (x `choose` k)) [k..])

-- | List all length-k increasing subsequences of 1,...,n
increasingLists :: Integral a => a -> a -> [[a]]
increasingLists n k = map (unrank (fromIntegral k)) [0..n `choose` k -1]

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

-- | Length n list of integers whos entries sum to r
sumRLists :: Integral a => a -> a -> [[a]]
sumRLists n r
          | r == 0 = [replicate (fromIntegral n) 0]
          | n == 1 = [[r]]
          | otherwise = concat [[l:ls | ls <- sumRLists (n-1) (r-l)] | l <- [0..r]]
