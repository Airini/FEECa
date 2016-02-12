{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module FEEC.Internal.MultiIndexTest where

import Properties
import Data.List
import FEEC.Utility.Combinatorics
import FEEC.Internal.MultiIndex
import Test.QuickCheck
import FEEC.Utility.Test
import Properties

instance Arbitrary MultiIndex where
    arbitrary = arbitraryMI 5 5

instance Arbitrary IncreasingList where
    arbitrary = increasingList 5 10

prop_zero :: Int -> Property
prop_zero n = n > 0 ==> degree (zero n) == 0

prop_unit :: SmallInt -> SmallInt' -> Bool
prop_unit (SmallInt n) (SmallInt' i) =
          degree (unit n i) == 1

prop_degreeR :: SmallInt -> SmallInt' -> Bool
prop_degreeR (SmallInt n) (SmallInt' r) =
    all ((r ==) . degree) (degreeR n r)

prop_extend :: IncreasingList -> MultiIndex -> Bool
prop_extend (IncreasingList sigma) mi =
      degree mi' == r &&
      map (toList mi' !!) sigma == toList mi
    where n = 10
          r = degree mi
          mi' = extend n sigma mi

prop_add :: MultiIndex -> MultiIndex -> Bool
prop_add mi1 mi2 = degree mi3 == degree mi1 + degree mi2
                   && range mi3 == sort (range mi1 `union` range mi2)
    where mi3 = add mi1 mi2

