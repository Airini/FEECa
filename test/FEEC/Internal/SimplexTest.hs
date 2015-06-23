{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module SimplexTest where


import Data.Maybe
import Data.List
import FEEC.Internal.Point
import FEEC.Internal.Simplex
import FEEC.Internal.Spaces
import FEEC.Internal.Vector
import FEEC.Utility.Combinatorics
import FEEC.Utility.Utility
import System.Random
import Test.QuickCheck(Arbitrary, arbitrary, quickCheck, (==>), Property)
import Test.QuickCheck.Gen(Gen, vectorOf)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as Q


data SubsimplexTest = SubsimplexTest Simplex Int Int deriving (Show)

--------------------------------------------------------------------------------
-- Random Simplices
--------------------------------------------------------------------------------

-- | Generate a random simplex.
instance Arbitrary Simplex where
    arbitrary = do Dimension n <- arbitrary
                   t <- vectorOf (n+1) $ nPoint n
                   return (Simplex [0..n] t)

-- | Return generator for a point of a given dimension.
nPoint :: Int -> Gen Point
nPoint n =  do l <- Q.vector n
               return (point l)
--------------------------------------------------------------------------------
-- Subsimplices
--------------------------------------------------------------------------------

-- | Arbitrary instance to test generation of subsimplices. Generates a full
-- | simplex of arbitrary dimension and integers k and i such that i is a valid
-- | index of a subsimplex of dimension k.
instance Arbitrary SubsimplexTest where
    arbitrary = do t <- arbitrary :: Gen Simplex
                   let n = topologicalDimension t
                   k <- Q.choose (0,n)
                   i <- Q.choose (0,max ((n+1) `choose` (k+1)-1) 0)
                   return (SubsimplexTest t k i)


-- | A subsimplex should contain only vertices contained in the supersimplex,
-- | and the length should be its topological dimension + 1.
prop_subsimplex :: SubsimplexTest -> Bool
prop_subsimplex (SubsimplexTest s@(Simplex _ l) k i) =
    (length subl == k+1) && all (`elem` l) subl
    where n         = topologicalDimension s
          subs      = subsimplex s k i
          subl      = vertices subs
          indexList = map (\x -> fromJust (elemIndex x l)) subl

-- | subsimplices should return (n+1) choose (k+1) subsimplices and the i:th
-- | subsimplex should be the same as subsimplex k i
prop_subsimplices :: SubsimplexTest -> Bool
prop_subsimplices (SubsimplexTest s@(Simplex _ l) k _) =
    (length subs == m) && all ithSubsimplexVertices [0..m-1]
    where n    = topologicalDimension s
          m    = (n+1) `choose` (k+1)
          subs = subsimplices s k
          ithSubsimplexVertices i =
              vertices (subs!!i) ==  vertices (subsimplex s k i)

-- | The volume of the reference simplices is 1/n!.
prop_vol :: Int -> Property
prop_vol i = (i > 0) ==> volume (referenceSimplex i) == 1.0 / (factorial' i)


--------------------------------------------------------------------------------
-- Integration
--------------------------------------------------------------------------------

-- Data type for constant functions.
data Constant = Constant Double

instance Function Constant Vector where
    type Values Constant Vector = Double
    deriv v h = (Constant 0)
    eval v (Constant c) = c

prop_vol_integral :: Simplex -> Bool
prop_vol_integral t = eqNum (volume t) (integral (div n 2) t (Constant 1))
    where n = topologicalDimension t

--------------------------------------------------------------------------------
-- Coordinates
--------------------------------------------------------------------------------

-- | Type to represent a point in the n-dimensional unit cube.
newtype Cubic = Cubic Point deriving (Show, Eq)

-- | Randomly pick a dimension n and a point from the n-dimensional unit
-- | cube.
instance Arbitrary Cubic where
    arbitrary = do n <- Q.choose (1,10)
                   cs <- Q.vector n
                   let transf l = zipWith (-) l (map (fromInt . truncate) l)
                   return (Cubic (point (map abs (transf cs))))

-- | Check that the transformation of a point in the n-dimensional unit cube is
-- | a valid point in barycentric coordinates, i.e. that all components are
-- |  positive and sum to one and that there are n + 1 components.
prop_cubicToBarycentric :: Cubic -> Bool
prop_cubicToBarycentric (Cubic p) =
    (prop_barycentric_range p') && (prop_barycentric_sum p') && (dim p' == dim p + 1)
        where p' = cubicToBarycentric p

prop_barycentric_range :: Point -> Bool
prop_barycentric_range p = (all (0 <=) cs) && (all (1 >=) cs)
    where cs = components (fromPoint p)

prop_barycentric_sum :: Point -> Bool
prop_barycentric_sum p = eqNum (sum cs) 1
    where cs = components (fromPoint p)
-- | Check that the unit vectors in barycentric coordinates reproduce the vertices
-- | of the simplex when transformed to cartesian coordinates.
prop_barycentricToCartesian :: Simplex -> Bool
prop_barycentricToCartesian t =
    map (barycentricToCartesian t) ps == vertices t
        where n = geometricalDimension t
              ps = [toPoint $ unitVector (n+1) i | i <- [0..n]]

main = do quickCheck (prop_subsimplex :: SubsimplexTest -> Bool)
          quickCheck (prop_subsimplices :: SubsimplexTest -> Bool)
          quickCheck prop_vol_integral
