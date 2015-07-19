{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module PolynomialTest where

import Test.QuickCheck
import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Polynomial
import FEEC.Internal.Spaces



arbitraryMI :: Int -> Gen MI.MultiIndex
arbitraryMI n = do l <- vector n
                   let m = minimum l
                   return (MI.multiIndex (map (m +) l))

instance (Ring r, Arbitrary r) => Arbitrary (Polynomial r)
    where arbitrary = do n <- choose (1,5)
                         r <- choose (0,10)
                         mis <- listOf (arbitraryMI n)
                         return (constant addId)
