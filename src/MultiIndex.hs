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

-- | Create a multi-index from a given list of integers.
multiIndex :: [Integer] -> MultiIndex
multiIndex l = ZipList l

-- | Degree of a multi-index, i.e. the sum of all indices
degMI :: Integral a => MultiIndex -> a
degMI = fromInteger . sum . getZipList

-- | Transform multi-index into list
toListMI :: Integral a => MultiIndex -> [a]
toListMI = map fromInteger . getZipList

-- | Degree zero multi-index
zeroMI ::Int -> MultiIndex
zeroMI n = ZipList (replicate n 0)

-- | Degree one multi-index with i-th element equal to one and all other zero
oneMI :: Int -> Int -> MultiIndex
oneMI n i = ZipList $ concat [replicate i 0,[1],replicate (n-i-1) 0]

-- | Decrease element in multi-index
decMI :: Integral a =>  Int -> ZipList a -> ZipList a
decMI i alpha  = pure f <*> ZipList [0..] <*> alpha
    where f j a = if j == i then max 0 (a-1) else a

-- | Add two multi-indices
addMI :: (Integral a) => ZipList a -> ZipList a -> ZipList a
addMI = liftA2 (+)

-- | Generalized binomial coefficients for multi-indices as defined in the paper
-- | by Kirby.
chooseMI :: (Integral a) => ZipList a -> ZipList a -> a
chooseMI a b = product $ getZipList $ liftA2 choose a b

-- | Generalized factorial for multi-indices
factorialMI :: (Bits a, Integral a) => MultiIndex -> a
factorialMI = product . map factorial . toListMI

-- | List of all length n multi-indices of degree r
degRMI :: Integral a => a -> a -> [MultiIndex]
degRMI n r = map ZipList $ sumRLists (fromIntegral n) (fromIntegral r)

-- | Checks if the first n indices of the multi-index are zero
isZeroMI :: Int -> MultiIndex -> Bool
isZeroMI n = all (0 ==) . take n . toListMI
