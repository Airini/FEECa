module MultiIndex where

import Spaces(Dimensioned(..))
import Data.Traversable
import Data.Bits
import Combinatorics(sumRLists)
import Math.Combinatorics.Exact.Binomial
import Math.Combinatorics.Exact.Factorial
import Control.Applicative(Applicative(..), ZipList(..), liftA, liftA2)

type MultiIndex = ZipList Integer

instance Dimensioned (ZipList a) where
    dim = length . getZipList

-- | Degree of a multi-index, i.e. the sum of all indices
deg :: Integral a => MultiIndex -> a
deg = fromInteger . sum . getZipList

-- | Transform multi-index into list
toList :: Integral a => MultiIndex -> [a]
toList = map fromInteger . getZipList

-- | Degree zero multi-index
zero ::Int -> MultiIndex
zero n = ZipList (replicate n 0)

-- | Degree one multi-index with i-th element equal to one and all other zero
one :: Int -> Int -> MultiIndex
one n i = ZipList $ concat [replicate i 0,[1],replicate (n-i-1) 0]

-- | Decrease element in multi-index
dec :: Integral a =>  Int -> ZipList a -> ZipList a
dec i alpha  = pure f <*> ZipList [0..] <*> alpha
    where f j a = if j == i then max 0 (a-1) else a

-- | Add two multi-indices
add :: (Integral a) => ZipList a -> ZipList a -> ZipList a
add = liftA2 (+)

-- | Generalized binomial coefficients for multi-indices as defined in the paper
-- | by Kirby.
choose :: (Integral a) => ZipList a -> ZipList a -> a
choose a b = product $ getZipList $ liftA2 choose a b

-- | Generalized factorial for multi-indices
factorial :: (Bits a, Integral a) => MultiIndex -> a
factorial = product . map factorial . toList

-- | List of all length n multi-indices of degree r
degR :: Integral a => a -> a -> [MultiIndex]
degR n r = map ZipList $ sumRLists (fromIntegral n) (fromIntegral r)

-- | Checks if the first n indices of the multi-index are zero
isZero :: Int -> MultiIndex -> Bool
isZero n = all (0 ==) . take n . toList
