{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module FEECa.Internal.MultiIndexTest where

import Data.List (sort, union)
import Control.Monad (liftM2)

import FEECa.Utility.Combinatorics
import FEECa.Internal.MultiIndex

import FEECa.Utility.Test
import Properties
import Test.QuickCheck

{-
instance Arbitrary MultiIndex where
    arbitrary = arbitraryMI 5 5
-}

testableMI = arbitraryMI 5 5

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

prop_extend :: IncreasingList -> Property
prop_extend (IncreasingList sigma) = forAll testableMI $ \mi ->
    let mi' = extend n sigma mi
    in    degree mi' == degree mi
        && map (toList mi' !!) sigma == toList mi
    where n = 10
          --r = degree mi

prop_add :: Property
prop_add = forAll (liftM2 (,) testableMI testableMI) $ \(mi1, mi2) ->
    let mi3 = add mi1 mi2
    in    degree mi3 == degree mi1 + degree mi2
        && range mi3 == sort (range mi1 `union` range mi2)


return []
testMI = $quickCheckAll

