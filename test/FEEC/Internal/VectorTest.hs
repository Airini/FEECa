{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module FEEC.Internal.VectorTest (
                                 arbitraryVector
                                ) where

import FEEC.Internal.Spaces
import FEEC.Internal.Vector
import FEEC.Utility.Utility
import Test.QuickCheck (Arbitrary(..), (==>), Gen, Property, quickCheck)
import qualified Test.QuickCheck as Q(vector)

--------------------------------------------------------------------------------
-- Generate random 4-dimensional vectors as a radom list of doubles.
--------------------------------------------------------------------------------
n = 4

instance EuclideanSpace (Vector r) r => Arbitrary (Vector r) where
    arbitrary = arbitraryVector n

arbitraryVector :: EuclideanSpace (Vector r) r => Int -> Gen (Vector r)
arbitraryVector n = do cs <- Q.vector n
                       return (fromDouble' cs)
--------------------------------------------------------------------------------
-- The dimension of the vector should be the length of the list passed to the
-- constructor.
--------------------------------------------------------------------------------
prop_dim :: [Double] -> Bool
prop_dim l = dim v == length l
    where v = vector l

--------------------------------------------------------------------------------
-- 'toList' should return the list that a vector v was constructed with.
--------------------------------------------------------------------------------

prop_toList :: [Double] -> Bool
prop_toList l = toList v == l
    where v = vector l

--------------------------------------------------------------------------------
-- Using 'apply' with the identity functions should leave the vector unchanged.
--------------------------------------------------------------------------------

prop_apply :: Eq (Vector f) => Vector f -> Bool
prop_apply v = (apply v id) == v

--------------------------------------------------------------------------------
-- The dot product must satisfy the properties of an inner product defined
-- over a real vector space.
--------------------------------------------------------------------------------

prop_dot :: (EuclideanSpace v r, Ord r) => r -> v -> v -> v -> Bool
prop_dot c u v w = prop_symmetric dot u v
                   && prop_linear dot c u v w
                   && prop_pos_def dot u

--------------------------------------------------------------------------------
-- Symmetry
--------------------------------------------------------------------------------

prop_symmetric :: Eq b => ( a -> a -> b ) -> a -> a -> Bool
prop_symmetric f a1 a2 = (f a1 a2) == (f a2 a1)

--------------------------------------------------------------------------------
-- Linearity
--------------------------------------------------------------------------------

prop_linear :: EuclideanSpace v r => (v -> v -> r) -> r -> v -> v -> v -> Bool
prop_linear f c u v w = eqNum (mul c (f u v)) (f (sclV c v) u)
                            && eqNum (f (addV u v) w) (add (f u w)  (f v w))

--------------------------------------------------------------------------------
-- Positive-definiteness
--------------------------------------------------------------------------------

prop_pos_def :: (EuclideanSpace v r, Ord r) => (v -> v -> r) -> v -> Bool
prop_pos_def f u = (f u u) >= addId
                   && if (f u u) == addId then u == zeroV u else True
