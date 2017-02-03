{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FEECa.Internal.SimplexTest (
    arbitrarySimplex, arbitrarySubsimplex, testSimplex
) where


import Control.Monad
import Data.Maybe
import Data.List

import FEECa.Internal.Simplex
import FEECa.Internal.Spaces
import FEECa.Internal.Vector
import FEECa.Internal.VectorTest

import FEECa.Utility.Combinatorics
import FEECa.Utility.Utility
import FEECa.Utility.Test

import Test.QuickCheck  ( Arbitrary, arbitrary, (==>), Property,
                          Gen, vectorOf, quickCheckAll )
import qualified Test.QuickCheck as Q


data SubsimplexTest v = SubsimplexTest (Simplex v) Int Int
  deriving (Show)

--------------------------------------------------------------------------------
-- Random Simplices
--------------------------------------------------------------------------------

-- | Generate random simplex of dimesion 1 <= n <= 10.
instance (EuclideanSpace v, Arbitrary v) => Arbitrary (Simplex v) where
  arbitrary = Q.choose (1,6) >>= arbitrarySimplex


--------------------------------------------------------------------------------
-- Subsimplices
--------------------------------------------------------------------------------

-- | Arbitrary instance to test generation of subsimplices. Generates a full
-- | simplex of arbitrary dimension and integers k and i such that i is a valid
-- | index of a subsimplex of dimension k.
instance (EuclideanSpace v, Arbitrary v) => Arbitrary (SubsimplexTest v) where
  arbitrary = do
    t <- arbitrary
    let n = topologicalDimension t
    k <- Q.choose (0, n)
    i <- Q.choose (0, max ((n+1) `choose` (k+1)-1) 0)
    return (SubsimplexTest t k i)


-- | A subsimplex should contain only vertices contained in the supersimplex,
-- | and the length should be its topological dimension + 1.
prop_subsimplex :: SubsimplexTest (Vector Double) -> Bool
prop_subsimplex = pSubsimplex

pSubsimplex :: Eq v => SubsimplexTest v -> Bool
pSubsimplex (SubsimplexTest s@(Simplex _ l) k i) =
    length subl == k+1 && all (`elem` l) subl
  where n     = topologicalDimension s
        subs  = subsimplex s k i
        subl  = vertices subs

-- | subsimplices should return (n+1) choose (k+1) subsimplices and the i:th
-- | subsimplex should be the same as subsimplex k i
prop_subsimplices :: SubsimplexTest (Vector Double) -> Bool
prop_subsimplices = pSubsimplices

pSubsimplices :: Eq v => SubsimplexTest v -> Bool
pSubsimplices (SubsimplexTest s@(Simplex _ l) k _) =
    length subs == m && all ithSubsxVerts [0..m-1]
  where n    = topologicalDimension s
        m    = (n+1) `choose` (k+1)
        subs = subsimplices s k
        ithSubsxVerts i = vertices (subs!!i) == vertices (subsimplex s k i)

-- | The extension of a subsimplex should have full dimensionality and non-zero
-- | volume.
prop_extend_subsimplex :: SubsimplexTest (Vector Double) -> Bool
prop_extend_subsimplex = pExtendSubsimplex

pExtendSubsimplex :: ( Show v, EuclideanSpace v) => SubsimplexTest v -> Bool
pExtendSubsimplex (SubsimplexTest s@(Simplex _ l) k i) =
    volume t /= addId && topologicalDimension t == geometricalDimension t
  where t = extendSimplex (subsimplex s k i)


-- | The simplex obtained from subsimplex should be the same
prop_face :: SubsimplexTest (Vector Double) -> Bool
prop_face = pFace

pFace :: EuclideanSpace v => SubsimplexTest v -> Bool
pFace (SubsimplexTest t k i) = subs == face t (sigma subs)
  where subs = subsimplex t k i

{-prop_face_vector_double :: SubsimplexTest (Vector Double) -> Bool
-- prop_face_vector_double = prop_face
-}

--------------------------------------------------------------------------------
-- Integration
--------------------------------------------------------------------------------

-- Data type for constant functions.
data Constant a = Constant a

instance (EuclideanSpace v, r ~ Scalar v) => Function (Constant r) v where
  derive v h = Constant (fromDouble 0.0)
  evaluate v (Constant c) = c

prop_vol_integral :: Simplex (Vector Double) -> Bool
prop_vol_integral = pVolIntegral

pVolIntegral :: EuclideanSpace v => Simplex v -> Bool
pVolIntegral t = eqNum (volume t) (integrate 2 t (Constant (fromDouble 1.0)))
  where n = topologicalDimension t  -- XXX: 2, 1, n??

-- TODO: perhaps add check that simPos is satisfied (if that is an invariant)
simPos :: Simplex (Vector Double) -> Bool
simPos s = volume s >= 0



--------------------------------------------------------------------------------
-- Coordinates
--------------------------------------------------------------------------------

-- | Type to represent a point in the n-dimensional unit cube.
newtype Cubic v r = Cubic v deriving (Show, Eq)

-- | Randomly pick a dimension n and a point from the n-dimensional unit
-- | cube.
instance (EuclideanSpace v, r ~ Scalar v) => Arbitrary (Cubic v r) where
  arbitrary = do
      let intBelow   = truncate :: Double -> Integer
          restrict x = sub x ((fromInt . intBelow) x)
      n  <- Q.choose (1,10)
      liftM (Cubic . fromDouble') $
        Q.vectorOf n $ liftM (restrict . abs) arbitrary

-- | Check that the transformation of a point in the n-dimensional unit cube is
-- | a valid point in barycentric coordinates, i.e. that all components are
-- |  positive and sum to one and that there are n + 1 components.
prop_cubicToBarycentric :: Cubic (Vector Double) Double -> Bool
prop_cubicToBarycentric = pCubicToBarycentric

pCubicToBarycentric :: (EuclideanSpace v, r ~ Scalar v)
                    => Cubic v r -> Bool
pCubicToBarycentric (Cubic v) =
        pBarycentricRange v' && pBarycentricSum v' && (dim v' == dim v + 1)
    where v' = cubicToBarycentric v

--prop_barycentric_range :: Vector Double -> Bool
--prop_barycentric_range = pBarycentricRange

pBarycentricRange :: EuclideanSpace v => v -> Bool
pBarycentricRange v = all (0 <=) cs && all (1 >=) cs
    where cs = toDouble' v
-- XXX: changed Cubic generator (meant to be the standard unit cube)

--prop_barycentric_sum :: Vector Double -> Bool
--prop_barycentric_sum = pBarycentricSum

pBarycentricSum :: EuclideanSpace v => v -> Bool
pBarycentricSum v = eqNum (sum (toDouble' v)) 1.0

-- | Check that the unit vectors in barycentric coordinates reproduce the vertices
-- | of the simplex when transformed to cartesian coordinates.
prop_barycentricToCartesian :: Simplex (Vector Double) -> Bool
prop_barycentricToCartesian = pBarycentricToCartesian

pBarycentricToCartesian :: EuclideanSpace v => Simplex v -> Bool
pBarycentricToCartesian t =
        map (barycentricToCartesian t) vs == vertices t
    where n = geometricalDimension t
          vs = [unitVector (n+1) i | i <- [0..n]]

-- TODO: Add testing for barycentric coordinates of subsimplices.

--main = do quickCheck (prop_subsimplex :: SubsimplexTest v -> Bool)
--          quickCheck (prop_subsimplices :: SubsimplexTest v -> Bool)
--          quickCheck prop_vol_integral

return []
testSimplex = $quickCheckAll



{-
type VectorD = Vector Double
type SimplexD = Simplex (Vector Double)

t :: SimplexD
t = referenceSimplex 3

t1 = subsimplex t 2 1
vs = spanningVectors t1

main = quickCheck (prop_extend_subsimplex :: SubsimplexTest VectorD -> Bool)

t3 = Simplex {sigma = [0,1,2,3,4], vertices = [Vector {components = [0.0,0.0,0.0,0.0]},Vector {components = [0.0,0.0,0.0,0.0]},Vector {components = [0.0,0.0,0.0,0.0]},Vector {components = [0.0,0.0,0.0,0.0]},Vector {components = [0.0,0.0,0.0,0.0]}]}
-}

