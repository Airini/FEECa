module FEEC.TestUtility where


import qualified FEEC.Internal.MultiIndex as MI
import Test.QuickCheck
import Data.List

------------------------------------------------------------------------------
-- Auxiliary Int type for randomly generated values between 0 and 10.
------------------------------------------------------------------------------

data SmallInt = SmallInt Int

instance Arbitrary SmallInt where
    arbitrary = do i <- choose (1,10)
                   return (SmallInt i)

instance Show SmallInt where
    show (SmallInt i) = show i

------------------------------------------------------------------------------
-- Auxiliary Int type for randomly generated values between 1 and 10.
------------------------------------------------------------------------------

data SmallInt' = SmallInt' Int

instance Arbitrary SmallInt' where
    arbitrary = do i <- choose (1,10)
                   return (SmallInt' i)

instance Show  SmallInt' where
    show (SmallInt' i) = show i

------------------------------------------------------------------------------
-- Generate a random multi-index of given dimension and degree r.
------------------------------------------------------------------------------

arbitraryMI :: Int -> Int -> Gen MI.MultiIndex
arbitraryMI n r = elements ( MI.degreeR n r )

------------------------------------------------------------------------------
-- Data type for increasing lists to represent subsets.
------------------------------------------------------------------------------

data IncreasingList = IncreasingList [Int]

instance Show IncreasingList where
    show (IncreasingList l) = show l

unique :: Eq a => [a] -> [a]
unique [] = []
unique (l:ls) = l : (filter (l /=) (unique ls))

------------------------------------------------------------------------------
-- Generate an increasing list as a random sublist of [0..n].
------------------------------------------------------------------------------
increasingList :: Int -> Int -> Gen IncreasingList
increasingList k n = do l <- infiniteListOf (choose (0,n))
                        let l' = take k (unique l)
                        return $ IncreasingList (sort l')
