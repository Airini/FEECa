{-# LANGUAGE TypeFamilies #-}

module Vector(Vector(Vector),
              vector,
              toList) where

import Spaces
import Text.PrettyPrint
import Print

-- | Vectors in R^n
data Vector = Vector [Double] deriving (Eq)

instance RenderVector Vector where
    dim = vspaceDim
    components = toList

instance Show Vector where
    show v = "Vector:\n" ++ (show $ printVector 2 v)

-- | Create vector from list of components.
vector :: [Double] -> Vector
vector = Vector

-- | Return vector components as list.
toList :: Vector -> [Double]
toList (Vector l) = l

instance VectorSpace Vector where
  type Fieldf Vector   = Double
  vspaceDim (Vector l) = length l
  addV (Vector l1) (Vector l2) = Vector $ zipWith (+) l1 l2
  sclV c (Vector l) = Vector $ map (c*) l

