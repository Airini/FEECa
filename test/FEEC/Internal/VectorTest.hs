{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module FEEC.Internal.VectorTest where


import FEEC.Internal.Vector
import FEEC.Internal.Spaces
import FEEC.Utility.Utility
import Test.QuickCheck (Arbitrary(..), (==>), Property, quickCheck)
import qualified Test.QuickCheck as Q(vector)

-- | Generate random 4-dimensional vectors as a radom list of doubles.
instance Arbitrary Vector where
    arbitrary = do components <- (Q.vector 4)
                   return (vector components)

-- | The dimension of the vector should be the length of the list passed to the
-- | constructor.
prop_dim :: [Double] -> Bool
prop_dim l = dim v == length l
    where v = vector l

-- | 'toList' should return the list that a vector v was constructed with.
prop_toList :: [Double] -> Bool
prop_toList l = toList v == l
    where v = vector l

-- | Using 'apply' with the identity functions should leave the vector unchanged.
prop_apply :: Vector -> Bool
prop_apply v = (apply v id) == v

-- | The dot product must satisfy the properties of an inner product defined
-- | over a real vector space.
prop_dot :: Double -> Vector -> Vector -> Vector -> Bool
prop_dot c u v w = prop_symmetry dot u v
                   && prop_linearity dot c u v w
                   && prop_pos_def dot u

-- | Symmetry
prop_symmetry :: (VectorSpace v, RealFloat (Scalar v))
               => (v -> v -> Scalar v)
               -> v
               -> v
               -> Bool
prop_symmetry f u v = eqNum (f u v) (f v u)

-- | Linearity
prop_linearity :: (VectorSpace v, RealFloat (Scalar v))
                => (v -> v -> Scalar v)
                -> Scalar v
                -> v
                -> v
                -> v
                -> Bool
prop_linearity f c u v w = eqNum (mul c (f u v)) (f (sclV c v) u)
                            && eqNum (f (addV u v) w) (add (f u w)  (f v w))

-- | Positive-definiteness
prop_pos_def :: (VectorSpace v, Eq v, RealFloat (Scalar v))
              => (v -> v -> Scalar v)
              -> v
              -> Bool
prop_pos_def f u = (f u u) >= addId
                   && if (f u u) == addId then u == zeroV u else True

-- | The unit vectors must be orthonormal.
prop_unitVector :: Int -> Int -> Int -> Property
prop_unitVector n i j = (n > 0) ==>
    dot (unitVector n i') (unitVector n j') == fromIntegral (delta i' j')
    where i' = i `mod` n
          j' = j `mod` n

delta :: Int -> Int -> Int
delta i j
      | i == j = 1
      | otherwise = 0
