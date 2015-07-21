{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module PolynomialTest where


import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Internal.Vector
import FEEC.Polynomial
import FEEC.Internal.Spaces
import FEEC.Utility.Test
import qualified Test.QuickCheck as Q
import Properties

n :: Int
n = 4

instance (Ring r, Q.Arbitrary r) => Q.Arbitrary (Polynomial r)
    where arbitrary = do r <- Q.choose (0,10)
                         mis <- Q.listOf (arbitraryMI n r)
                         cs <- Q.listOf Q.arbitrary
                         return $ polynomial (zip cs mis)

instance Field f => Q.Arbitrary (Vector f) where
    arbitrary = do l <- Q.vector n
                   return $ vector (map fromDouble l)

prop_arithmetic :: (EuclideanSpace v r, Q.Arbitrary v, Q.Arbitrary r)
                => Polynomial r
                -> Polynomial r
                -> v
                -> Bool
prop_arithmetic p1 p2 v = -- prop_operator_commutativity add add (evaluate v) p1 p1
                    prop_operator_commutativity mul mul (evaluate v) p1 p1

prop_arithmetic_rf :: Polynomial Rational
                   -> Polynomial Rational
                   -> Vector Rational
                   -> Bool
prop_arithmetic_rf = prop_arithmetic


p1 :: Polynomial Rational
p1 = polynomial [(((-7763204307700) / 9103688388447),MI.multiIndex [0,0,1,1,1]),(((-5768714918348) / 1622622387537),MI.multiIndex [0,2,1,0,0])]
p2 :: Polynomial Rational
p2 = polynomial [(((-607364476886) / 44357178205), MI.multiIndex [1,0,0,0,0]), ((10368138976756 / 5789853960481) , MI.multiIndex [1,0,0,0,0])]
v :: Vector Rational
v = vector [(-1983254388243891) / 2251799813685248,(-3659286520981969) / 140737488355328,5360377639166411 / 70368744177664,1295570148677743 / 1125899906842624]
