module FEEC.Internal.MultiIndex where

import Data.Traversable
import Data.Bits
import FEEC.Internal.Spaces(Dimensioned(..))
import FEEC.Utility.Combinatorics(sumRLists)
import qualified Math.Combinatorics.Exact.Binomial as CBin
import qualified Math.Combinatorics.Exact.Factorial as CFac
import Control.Applicative(Applicative(..), ZipList(..), liftA, liftA2)

type MultiIndex = ZipList Int

instance Dimensioned (ZipList a) where
    dim mi = (length (getZipList mi)) - 1

-- | Create a multi-index from a given list of integers.
multiIndex :: [Int] -> MultiIndex
multiIndex l = ZipList l

-- | Degree of a multi-index, i.e. the sum of all indices
deg :: Integral a => MultiIndex -> a
deg = fromIntegral . sum . getZipList

-- | Extend multi-index from from a face to a simplex.
extend :: Int -> [Int] -> MultiIndex -> MultiIndex
extend n f mi = multiIndex $ extend' n (diff (-1) f) (toList mi)

extend' :: Int -> [Int] -> [Int] -> [Int]
extend' n (i:is) (j:js) = (replicate (i-1) 0) ++ (j : (extend' (n - i) is js))
extend' n [] [] = replicate (n + 1) 0

diff :: Int -> [Int] -> [Int]
diff i (j:js) = (j - i) : (diff j js)
diff i [] = []

-- | Transform multi-index into list
toList :: Integral a => MultiIndex -> [a]
toList = map fromIntegral . getZipList

-- | Degree zero multi-index
zero ::Int -> MultiIndex
zero n = ZipList (replicate n 0)

-- | Degree one multi-index with i-th element equal to one and all other zero
one :: Int -> Int -> MultiIndex
one n i = ZipList $ concat [replicate i 0,[1],replicate (n-i) 0]

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
choose a b = product $ getZipList $ liftA2 CBin.choose a b

-- | Generalized factorial for multi-indices
factorial :: (Bits a, Integral a) => MultiIndex -> a
factorial = product . map CFac.factorial . toList

-- | List of all dimension n (length n + 1) multi-indices of degree r
degR :: Integral a => a -> a -> [MultiIndex]
degR n r = map ZipList $ sumRLists (fromIntegral (n + 1)) (fromIntegral r)

-- | Checks if the first n indices of the multi-index are zero
isZero :: Int -> MultiIndex -> Bool
isZero n = all (0 ==) . take n . toList

-- | Check whether a given multi-index is valid.
valid :: MultiIndex -> Bool
valid mi = all (0 <=) (toList mi)

-- | List indices of the multi-index that are non-zero.
range :: MultiIndex -> [Int]
range = range' 0

range' :: Int -> MultiIndex -> [Int]
range' i (ZipList (l:ls))
      | l /= 0 = i:(range' (i+1) (ZipList ls))
      | otherwise = range' (i+1) (ZipList ls)
range' _ (ZipList []) = []
