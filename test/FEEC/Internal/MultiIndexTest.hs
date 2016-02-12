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

{- TODO
  ERROR when testing. Two examples:

*** Failed! (after 1 test):
Exception:
  [2,3,4,5,6,8]
     ZipList {getZipList = [1,0,2,1,1]}extend: Dimensions of sigma and
     multi-index don't agree
     [2,3,4,5,6,8]
     ZipList {getZipList = [1,0,2,1,1]}

*** Failed! (after 1 test):
Exception:
  [0,1,6,7,8,10]
     ZipList {getZipList = [0,0,1,3,1]}extend: Dimensions of sigma and
     multi-index don't agree
     [0,1,6,7,8,10]
     ZipList {getZipList = [0,0,1,3,1]}
-}

prop_add :: MultiIndex -> MultiIndex -> Bool
prop_add mi1 mi2 = degree mi3 == degree mi1 + degree mi2
                   && range mi3 == sort (range mi1 `union` range mi2)
    where mi3 = add mi1 mi2

