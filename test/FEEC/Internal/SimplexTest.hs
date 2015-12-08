{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FEEC.Internal.SimplexTest(
                                 arbitrarySimplex
                                ) where


import Data.Maybe
import Data.List
import FEEC.Internal.Simplex
import FEEC.Internal.Spaces
import FEEC.Internal.Vector
import FEEC.Internal.VectorTest
import FEEC.Utility.Combinatorics
import FEEC.Utility.Utility
import System.Random
import Test.QuickCheck(Arbitrary, arbitrary, quickCheck, (==>), Property)
import Test.QuickCheck.Gen(Gen, vectorOf)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as Q

data SubsimplexTest v = SubsimplexTest (Simplex v) Int Int deriving (Show)

--------------------------------------------------------------------------------
-- Random Simplices
--------------------------------------------------------------------------------

-- | Generate a random simplex of given dimension.
arbitrarySimplex :: (EuclideanSpace v, Arbitrary v) => Int -> Gen (Simplex v)
arbitrarySimplex n = do l <- Q.infiniteListOf (vectorOf (n+1) (arbitraryVector n))
                        let simplices = map (Simplex [0..n]) l
                        return (head simplices)

-- | Generate random simplex of dimesion 1 <= n <= 10.
instance (EuclideanSpace v, Arbitrary v) => Arbitrary (Simplex v) where
    arbitrary = do n <- Q.choose (1, 6)
                   arbitrarySimplex n

--------------------------------------------------------------------------------
-- Subsimplices
--------------------------------------------------------------------------------

-- | Arbitrary instance to test generation of subsimplices. Generates a full
-- | simplex of arbitrary dimension and integers k and i such that i is a valid
-- | index of a subsimplex of dimension k.
instance (EuclideanSpace v, Arbitrary v) => Arbitrary (SubsimplexTest v) where
    arbitrary = do t <- arbitrary
                   let n = topologicalDimension t
                   k <- Q.choose (0,n)
                   i <- Q.choose (0,max ((n+1) `choose` (k+1)-1) 0)
                   return (SubsimplexTest t k i)


-- | A subsimplex should contain only vertices contained in the supersimplex,
-- | and the length should be its topological dimension + 1.
prop_subsimplex :: Eq v => SubsimplexTest v -> Bool
prop_subsimplex (SubsimplexTest s@(Simplex _ l) k i) =
    (length subl == k+1) && all (`elem` l) subl
    where n         = topologicalDimension s
          subs      = subsimplex s k i
          subl      = vertices subs

-- | subsimplices should return (n+1) choose (k+1) subsimplices and the i:th
-- | subsimplex should be the same as subsimplex k i
prop_subsimplices :: Eq v => SubsimplexTest v -> Bool
prop_subsimplices (SubsimplexTest s@(Simplex _ l) k _) =
    (length subs == m) && all ithSubsimplexVertices [0..m-1]
    where n    = topologicalDimension s
          m    = (n+1) `choose` (k+1)
          subs = subsimplices s k
          ithSubsimplexVertices i =
              vertices (subs!!i) ==  vertices (subsimplex s k i)

-- | The extension of a subsimplex should have full dimensionality and non-zero
-- | volume.
prop_extend_subsimplex :: EuclideanSpace v => SubsimplexTest v -> Bool
prop_extend_subsimplex (SubsimplexTest s@(Simplex _ l) k i) =
    (volume t' /= addId) && ((topologicalDimension t') == (geometricalDimension t'))
        where t' = extendSimplex (subsimplex s k i)

--------------------------------------------------------------------------------
-- Integration
--------------------------------------------------------------------------------

-- Data type for constant functions.
data Constant a = Constant a

instance (EuclideanSpace v, r ~ Scalar v) => Function (Constant r) v where
    derive v h = (Constant (fromDouble 0.0))
    evaluate v (Constant c) = c

prop_vol_integral :: EuclideanSpace v => Simplex v -> Bool
prop_vol_integral t = eqNum (volume t) (integrate 2 t (Constant (fromDouble 1.0)))
     where n = topologicalDimension t

--------------------------------------------------------------------------------
-- Coordinates
--------------------------------------------------------------------------------

-- | Type to represent a point in the n-dimensional unit cube.
newtype Cubic v r = Cubic v deriving (Show, Eq)

-- | Randomly pick a dimension n and a point from the n-dimensional unit
-- | cube.
instance (EuclideanSpace v, r ~ Scalar v) => Arbitrary (Cubic v r) where
    arbitrary = do n <- Q.choose (1,10)
                   cs <- Q.vector n
                   let transf l = zipWith (sub) l (map (fromInt . truncate') l)
                   return (Cubic (fromDouble' (transf cs)))
        where truncate' :: Double -> Integer
              truncate' = truncate

-- | Check that the transformation of a point in the n-dimensional unit cube is
-- | a valid point in barycentric coordinates, i.e. that all components are
-- |  positive and sum to one and that there are n + 1 components.
prop_cubicToBarycentric :: (EuclideanSpace v, r ~ Scalar v)
                           => Cubic v r -> Bool
prop_cubicToBarycentric (Cubic v) = (prop_barycentric_range v') && (prop_barycentric_sum v') && (dim v' == dim v + 1)
        where v' = cubicToBarycentric v

prop_barycentric_range :: EuclideanSpace v => v -> Bool
prop_barycentric_range v = (all (0 <=) cs) && (all (1 >=) cs)
    where cs = toDouble' v

prop_barycentric_sum :: EuclideanSpace v => v -> Bool
prop_barycentric_sum v = eqNum (sum ( toDouble' v )) 1.0

-- | Check that the unit vectors in barycentric coordinates reproduce the vertices
-- | of the simplex when transformed to cartesian coordinates.
prop_barycentricToCartesian :: EuclideanSpace v
                            => Simplex v
                            -> Bool
prop_barycentricToCartesian t =
    map (barycentricToCartesian t) vs == vertices t
        where n = geometricalDimension t
              vs = [unitVector (n+1) i | i <- [0..n]]


-- TODO: Add testing for barycentric coordinates of subsimplices.

--main = do quickCheck (prop_subsimplex :: SubsimplexTest v -> Bool)
--          quickCheck (prop_subsimplices :: SubsimplexTest v -> Bool)
--          quickCheck prop_vol_integral

type VectorD = Vector Double
type SimplexD = Simplex (Vector Double)

t :: SimplexD
t = referenceSimplex 3

t1 = subsimplex t 2 1
vs = spanningVectors t1

main = do quickCheck (prop_extend_subsimplex :: SubsimplexTest VectorD -> Bool)

t3 = Simplex {sigma = [0,1,2,3,4], vertices = [Vector {components = [0.0,0.0,0.0,0.0]},Vector {components = [0.0,0.0,0.0,0.0]},Vector {components = [0.0,0.0,0.0,0.0]},Vector {components = [0.0,0.0,0.0,0.0]},Vector {components = [0.0,0.0,0.0,0.0]}]}
