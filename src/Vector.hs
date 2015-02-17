{-# LANGUAGE TypeFamilies #-}

module Vector(Vector(..),
              vector,
              toList,
              powV,
              dot,
              unitV,
              toPoint,
              fromPoint) where

import Spaces hiding (toList)
import Point
import Text.PrettyPrint
import Print


-- | Vectors in R^n
data Vector = Vector [Double] deriving (Eq)

instance RenderVector Vector where
    ncomps (Vector l) = length l
    components = toList

instance Dimensioned Vector where
    dim (Vector l) = length l

instance Show Vector where
    show v = "Vector:\n" ++ (show $ printVector 2 v)

instance VectorSpace Vector where
  type Fieldf Vector   = Double
  vspaceDim (Vector l) = length l
  addV (Vector l1) (Vector l2) = Vector $ zipWith (+) l1 l2
  sclV c (Vector l) = Vector $ map (c*) l

-- | Create vector from list of components.
vector :: [Double] -> Vector
vector = Vector

-- | Return vector components as list.
toList :: Vector -> [Double]
toList (Vector l) = l

-- | Point corresponding to given position vector.
toPoint :: Vector -> Point
toPoint (Vector l) = point l

-- | Position vector of given point.
fromPoint :: Point -> Vector
fromPoint (Point l) = Vector l

-- | Dot product
dot :: Vector -> Vector -> Double
dot (Vector l1) (Vector l2) = foldl (\s (x,y) -> s + x*y) 0  $ zip l1 l2

-- | ith unit vector in R^n
unitV :: Int -> Int -> Vector
unitV n i = Vector $ concat [(replicate (i) 0.0), [1.0], (replicate (n-i-1) 0.0)]

-- | Generalized power funciton for vectors. Given a list l of Int with
-- | the same length as the dimension of the vector v and components cs, the
-- | function computes the product of each component (cs!!i) raised to (l!!i)th
-- | power.
powV :: Vector -> [Integer] -> Double
powV (Vector cs) l = powVList cs l

powVList [] [] = mulId
powVList (v:vs) (i:is) = v ** (fromIntegral i) * powVList vs is
powVLint _ _ = error "powV: Lists do not have equal length"
