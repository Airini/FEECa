{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module SimplexTest where

import qualified Math.Combinatorics.Exact.Binomial as C

import Data.Maybe
import Data.List
import FEEC.Internal.Point
import FEEC.Internal.Simplex
import FEEC.Internal.Spaces
import FEEC.Utility.Utility
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import qualified Test.QuickCheck.Property as QCP
import qualified FEEC.Internal.Vector as V

data SubsimplexTest = SubsimplexTest Simplex Int Int

-- | Generate a random simplex.
instance Arbitrary Simplex where
    arbitrary = do Dimension n <- arbitrary
                   t <- vectorOf (n+1) $ nPoint n
                   return (Simplex [0..n] t)

-- | Arbitrary instance to test generation of subsimplices.
instance Arbitrary SubsimplexTest where
    arbitrary = do t <- arbitrary :: Gen Simplex
                   let n = topologicalDimension t
                   k <- choose(0,n)
                   i <- choose(0,max ((n+1) `C.choose` (k+1)-1) 0)
                   return (SubsimplexTest t k i)

nPoint :: Int -> Gen Point
nPoint n = do
  l <- vector n
  return (Point l)

-- A subsimplex should contain only vertices contained in the supersimplex,
-- and the length should be its topological dimension + 1.
prop_subsimplex :: SubsimplexTest -> Bool
prop_subsimplex (SubsimplexTest s@(Simplex _ l) k i) =
    (length subl == k+1) && all (`elem` l) subl
    where n         = topologicalDimension s
          subs      = subsimplex s k i
          subl      = vertices subs
          indexList = map (\x -> fromJust (elemIndex x l)) subl

-- subsimplices should return (n+1) choose (k+1) subsimplices and the i:th
-- subsimplex should be the same as subsimplex k i
prop_subsimplices :: SubsimplexTest -> Bool
prop_subsimplices (SubsimplexTest s@(Simplex _ l) k _) =
    (length subs == m) && all ithSubsimplexVertices [0..m-1]
    where n    = topologicalDimension s
          m    = (n+1) `C.choose` (k+1)
          subs = subsimplices s k
          ithSubsimplexVertices i =
              vertices (subs!!i) ==  vertices (subsimplex s k i)

-- Data type for constant functions.
data Constant = Constant Double

instance Function Constant V.Vector where
    type Values Constant V.Vector = Double
    deriv v h = (Constant 0)
    eval v (Constant c) = c

prop_vol_integral :: Simplex -> Bool
prop_vol_integral t = eqNum (volume t) (integral (div n 2) t (Constant 1))
    where n = topologicalDimension t

main = do quickCheck (prop_subsimplex :: SubsimplexTest -> Bool)
          quickCheck (prop_subsimplices :: SubsimplexTest -> Bool)
          quickCheck prop_vol_integral
