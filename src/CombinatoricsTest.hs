import Combinatorics
import qualified Math.Combinatorics.Exact.Binomial as Comb
import Test.QuickCheck
import Data.List (sort, (\\))

newtype OrderedIntPair = OrderedIntPair (Int, Int) deriving (Eq, Show)

instance Arbitrary OrderedIntPair where
    arbitrary = do
      k <- choose (1,10)
      n <- choose (0,100)
      return (OrderedIntPair (k, n))

newtype SmallInt = SmallInt Int deriving (Eq, Show)

instance Arbitrary SmallInt where
    arbitrary = do
        k <- choose (1,20)
        return (SmallInt k)

-- | Computing the rank of the unranked argument should reproduce the argument
prop_idempotency :: OrderedIntPair -> Bool
prop_idempotency (OrderedIntPair (k, n)) =
    n == rank (unrank k n)

-- | For each N the corresponding k-increasing list should be increasing
prop_increasing :: OrderedIntPair -> Bool
prop_increasing (OrderedIntPair (k,n)) = increasing (unrank k n)

increasing :: [Int] -> Bool
increasing (l:(m:ms)) = and [l<m,increasing ms]
increasing (l:_) = True
increasing [] = True

--- | For each length k the list with index 0 is just 0..k
prop_indexZero :: SmallInt -> Bool
prop_indexZero (SmallInt i) = (unrank i 0) == [1..i]

-- | For an k-increasing list, the list containing the removed elements of the
-- | k-1-sublists should reproduce the list
prop_sublists :: OrderedIntPair -> Bool
prop_sublists (OrderedIntPair (k,n)) = l ==  map head (map (l \\) subl)
    where l = unrank k n
          subl = map (unrank (k-1)) (sublists k n)

