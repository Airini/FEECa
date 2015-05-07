{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module PolynomialsTest where
import Data.Maybe
import Polynomials
import Test.QuickCheck as Q
import Simplex
import Point
import Vector
import Spaces
import qualified Numeric.LinearAlgebra.HMatrix as M

newtype ConvexComb4 = ConvexComb4 [Double] deriving Show

-- | Coefficient for the convex combination of 4 Q.vectors
instance Arbitrary ConvexComb4 where
    arbitrary = do
      l <- Q.vector 4 `suchThat` (\ l -> sum (map abs l) > 0)
      let labs = map abs l
          s = sum labs
          l' = map ((1/s)*) labs
      return (ConvexComb4 l')

-- | Generate arbitrary polyhedron with non-zero vertices.
instance Arbitrary Simplex where
    arbitrary = do
      ls <- infiniteListOf (Q.vector 3 `suchThat` (not . ([0,0,0] ==)))
      let l1 = head $ dropWhile (all (0==)) ls
          l2 = head $ dropWhile (not . linearIndep' [l1]) ls
          l3 = head $ dropWhile (not . linearIndep' [l1,l2]) ls
          l4 = ls !! 3
      return $ Simplex (map point [l1,l2,l3,l4])


-- | Test list of Q.vectors for linear independence.
linearIndep :: [Vector] -> Bool
linearIndep vs = m == M.rank a
    where m = length vs
          n = dim (head vs)
          a = (m M.>< n) (concatMap toList vs)

linearIndep' :: [[Double]] -> [Double] -> Bool
linearIndep' vs v = linearIndep (Vector.vector v : map Vector.vector vs)

-- | Defining property of barycentric coordinates: The barycentric coordinate
-- | function lambda_i takes value 1 on the vertex x_i of the simplex and 0 on
-- | all other vertices.
prop_barycentric :: Simplex -> Bool
prop_barycentric s@(Simplex l) = all equalsDelta [0..n]
    where n = topologicalDimension s
          equalsDelta i = all (\j -> lambda i (fromPoint (l!!j)) `eqNum` delta i j) [0..n]
          lambda i v = eval v (barycentricCoordinate s i)

-- FIXME: Inf.Loop detected!
-- | Barycentric coordinates should be positive everywhere inside the simplex.
prop_pos :: ConvexComb4 -> Simplex -> Bool
prop_pos (ConvexComb4 cs) s = all largerThan0 [0..3]
    where l = map fromPoint (vertices s)
          v = foldr (\ (c,v) s -> addV s (sclV c v)) (zeroV v) (zip cs l)
          lambda i v = eval v (barycentricCoordinate s i)
          largerThan0 i = lambda i v >= 0.0

delta :: Int -> Int -> Double
delta i j
      | i == j = 1
      | otherwise = 0

-- | Numerical equality accounting for round-off errors
eqNum :: Double -> Double -> Bool
eqNum a b = abs (a - b) < 2e-13
