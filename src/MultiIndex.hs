module MultiIndex where

import Spaces(Dimensioned(..))
import Data.Bits
import Math.Combinatorics.Exact.Binomial
import Math.Combinatorics.Exact.Factorial
import Control.Applicative(Applicative(..), ZipList(..), liftA, liftA2)

type MultiIndex = ZipList Integer

instance Dimensioned (ZipList a) where
    dim = length . getZipList

degMI :: Integral a => MultiIndex -> a
degMI = fromIntegral . sum . getZipList

toListMI :: Integral a => MultiIndex -> [a]
toListMI = (map fromIntegral) . getZipList

zeroMI ::Int -> MultiIndex
zeroMI n = ZipList (replicate n 0)

oneMI :: Int -> Int -> MultiIndex
oneMI n i = ZipList $ concat [replicate i 0,[1],replicate (n-i-1) 0]

-- | Decrease element in multi-index
decMI :: (Integral a) =>  Int -> ZipList a -> ZipList a
decMI i alpha  = (pure f) <*> (ZipList [0..]) <*> alpha
    where f j a = if (j == i) then max 0 (a-1) else a


-- | Add two multi-indices
addMI :: (Integral a) => ZipList a -> ZipList a -> ZipList a
addMI = liftA2 (+)

-- | Generalized binomial coefficients for multi-indices as defined in the paper
-- | by Kirby.
chooseMI :: (Integral a) => ZipList a -> ZipList a -> ZipList a
chooseMI = liftA2 choose

-- | Generalized factorial for multi-indices
factorialMI :: (Integral a, Bits a) => MultiIndex -> a
factorialMI = (foldl (*) 1) . (map factorial) . toListMI
