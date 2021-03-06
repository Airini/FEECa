{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module FEECa.Internal.VectorTest (
    testVector, arbitraryVector
) where

import FEECa.Internal.Spaces
import FEECa.Internal.Vector
import FEECa.Utility.Utility
import FEECa.Utility.Test

import Properties       ( prop_commutativity )
import Test.QuickCheck  ( Arbitrary(..), (==>), Gen, Property, quickCheckWithResult )
import Test.QuickCheck.Test           ( isSuccess )
import qualified Test.QuickCheck as Q ( vector )

testVector args = do
  ps <- mapM (quickCheckWithResult args) [ prop_dim, prop_toList ]
  dp <- quickCheckWithResult args
        (prop_dot :: Double -> Vector Double -> Vector Double -> Vector Double -> Bool)
  return $ all isSuccess (dp : ps)


--------------------------------------------------------------------------------
-- Generate random 4-dimensional vectors as a radom list of doubles.
--------------------------------------------------------------------------------
n = 4

instance (EuclideanSpace (Vector r), Arbitrary r) => Arbitrary (Vector r) where
  arbitrary = arbitraryVector n

--------------------------------------------------------------------------------
-- The dimension of the vector should be the length of the list passed to the
-- constructor.
--------------------------------------------------------------------------------
prop_dim :: [Double] -> Bool
prop_dim l = dim (vector l) == length l

--------------------------------------------------------------------------------
-- 'toList' should return the list that a vector v was constructed with.
--------------------------------------------------------------------------------

prop_toList :: [Double] -> Bool
prop_toList l = toList (vector l) == l


--------------------------------------------------------------------------------
-- The dot product must satisfy the properties of an inner product defined
-- over a real vector space.
--------------------------------------------------------------------------------

prop_dot :: (EuclideanSpace v, Ord r, r ~ Scalar v) => r -> v -> v -> v -> Bool
prop_dot c u v w = prop_symmetric dot u v
                && prop_linear    dot c u v w
                && prop_pos_def   dot u

--------------------------------------------------------------------------------
-- Symmetry
--------------------------------------------------------------------------------

prop_symmetric :: Eq b => ( a -> a -> b ) -> a -> a -> Bool
prop_symmetric f a1 a2 = f a1 a2 == f a2 a1

--------------------------------------------------------------------------------
-- Linearity
--------------------------------------------------------------------------------

prop_linear :: (EuclideanSpace v, r ~ Scalar v)
            => (v -> v -> r) -> r -> v -> v -> v -> Bool
prop_linear f c u v w = eqNum (mul c (f u v))  (f (sclV c v) u)
                     && eqNum (f (addV u v) w) (add (f u w) (f v w))

--------------------------------------------------------------------------------
-- Positive-definiteness
--------------------------------------------------------------------------------

prop_pos_def :: (EuclideanSpace v, Ord r, r ~ Scalar v)
                => (v -> v -> r) -> v -> Bool
prop_pos_def f u = f u u >= addId
                && (f u u /= addId || u == zeroV u)
