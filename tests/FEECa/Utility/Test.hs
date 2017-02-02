module FEECa.Utility.Test where

import Data.List      ( sort )
import Control.Monad  ( liftM )

import FEECa.Internal.Vector
import FEECa.Internal.Simplex
import FEECa.Internal.Spaces
import qualified FEECa.Internal.MultiIndex as MI

import qualified Test.QuickCheck as Q


------------------------------------------------------------------------------
-- Auxiliary Int type for randomly generated values between 0 and 10.
------------------------------------------------------------------------------

data SmallInt = SmallInt Int

instance Q.Arbitrary SmallInt where
  arbitrary = liftM SmallInt $ Q.choose (1,10)

instance Show SmallInt where
  show (SmallInt i) = show i

------------------------------------------------------------------------------
-- Auxiliary Int type for randomly generated values between 1 and 10.
------------------------------------------------------------------------------

data SmallInt' = SmallInt' Int

instance Q.Arbitrary SmallInt' where
  arbitrary = liftM SmallInt' $ Q.choose (1,10)

instance Show  SmallInt' where
  show (SmallInt' i) = show i

------------------------------------------------------------------------------
-- Generate a random multi-index of given dimension and degree r.
------------------------------------------------------------------------------

arbitraryMI :: Int -> Int -> Q.Gen MI.MultiIndex
arbitraryMI n r = Q.elements ( MI.degreeR n r )

------------------------------------------------------------------------------
-- Data type for increasing lists to represent subsets.
------------------------------------------------------------------------------

data IncreasingList = IncreasingList [Int]

instance Show IncreasingList where
  show (IncreasingList l) = show l

unique :: Eq a => [a] -> [a]
unique []     = []
unique (l:ls) = l : filter (l /=) (unique ls)

------------------------------------------------------------------------------
-- Generate an increasing list as a random sublist of [0..n].
------------------------------------------------------------------------------
increasingList :: Int -> Int -> Q.Gen IncreasingList
increasingList k n =
  liftM ( IncreasingList . sort . take k . unique )
        ( ( Q.infiniteListOf . Q.choose )  (0,n)  )

-- | Generate a random vector of given length.
arbitraryVector :: EuclideanSpace v => Int -> Q.Gen v
arbitraryVector = liftM fromDouble' . Q.vector

-- | Generate a random simplex of given dimension.
arbitrarySimplex :: (EuclideanSpace v, Q.Arbitrary v)
                 => Int -> Q.Gen (Simplex v)
arbitrarySimplex n =  t `Q.suchThat` ((addId /=) . volume)
  where t   = liftM simplex vs
        vs  = Q.vectorOf (n+1) (arbitraryVector n)

-- | Generate a random k-subsimplex of given dimension n.
arbitrarySubsimplex :: (EuclideanSpace v, Q.Arbitrary v)
                    => Int -> Int -> Q.Gen (Simplex v)
arbitrarySubsimplex k n = arbitrarySimplex n >>= Q.elements . (`subsimplices` k)
