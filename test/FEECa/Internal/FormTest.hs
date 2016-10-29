{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module FEECa.Internal.FormTest where

import Data.List

import Test.QuickCheck as Q
import FEECa.Internal.Form
import FEECa.Internal.Vector
import FEECa.Internal.Spaces
import FEECa.Utility.Combinatorics
import FEECa.Utility.Test

import Debug.Trace


-- TODO: inner + interior product properties


-- * Instantiated testable properties

-- -- | Tests batch of properties
-- main maxDim = do mapM_ quickCheck (checkList maxDim)
--                  quickCheck prop_antiComm

dx :: (EuclideanSpace v, r ~ Scalar v) => Int -> v -> r
dx i v = (toList v) !! (i - 1)

-- linear = LC [(1.0, [0,1])] :: LinearCombination Double [Int]
-- v1 = Vector [1.0, 2.0]
-- v2 = Vector [2.0, 1.0]

k = 2
n = 4

data ApplicationTest c v = ApplicationTest (Form c) [v]
    deriving (Show)

instance (EuclideanSpace v, Arbitrary r, Scalar v ~ r) => Arbitrary (ApplicationTest r v)
    where arbitrary = do n     <- Q.choose (1, 5)
                         k     <- Q.choose (0, n)
                         omega <- arbitraryForm k n
                         vs    <- Q.vectorOf k $ arbitraryVector n
                         return $ ApplicationTest omega vs

instance VectorSpace Rational where
  type Scalar Rational = Rational
  sclV = mul
  addV = add

instance Numeric Rational

--------------------------------------------------------------------------------
-- Linearity of forms.
--------------------------------------------------------------------------------

-- | Forms are linear in each vector argument.
prop_linearity :: (EuclideanSpace v, Numeric r, Scalar v ~ r)
               => ApplicationTest r v -> [r] -> Property
prop_linearity (ApplicationTest omega vs) cs = not (null cs) ==>
  val1 == mul (foldl mul mulId (take (length vs) cs')) val2
  where val1 = apply dx omega (zipWith sclV cs' vs)
        val2 = apply dx omega vs
        cs'  = cycle cs

--------------------------------------------------------------------------------
-- Anticommutativity of forms.
--------------------------------------------------------------------------------

-- | Permuting the vectors that are applied to a form should change the sign
-- | according to the parity of the permutation.
prop_anticommutativity :: (EuclideanSpace v, Numeric r, Scalar v ~ r)
                       => ApplicationTest r v -> Bool
prop_anticommutativity (ApplicationTest omega vs) =
  and $ [(apply dx omega vs) == (mul par (apply dx omega vs')) | (par, vs') <- ps]
  where ps      = zip signs (permutations vs)
        signs   = map parity' (permutations [1..length vs])
        parity' = (\i -> if (even i) then (fromInt' 1) else (fromInt' (-1))) . parity

--------------------------------------------------------------------------------
-- Simplification of forms.
--------------------------------------------------------------------------------

-- | The simplification of forms must not change their value.
prop_simplification :: (EuclideanSpace v, Numeric r, Scalar v ~ r)
                    => ApplicationTest r v -> Bool
prop_simplification (ApplicationTest omega vs) =
  apply dx omega vs == apply dx (combine omega) vs

--------------------------------------------------------------------------------
-- Interior Product.
--------------------------------------------------------------------------------

-- | Successively computing the interior product of a form and a list of vectors
-- | should be the same as applying the vector directly.
prop_interior :: (EuclideanSpace v, Numeric r, Scalar v ~ r, Show r, Show v)
                 => ApplicationTest r v -> Bool
prop_interior (ApplicationTest omega vs) =
  apply dx omega vs == apply undefined (foldl' (interiorProduct dx) omega vs) []

-- -- | Tests for algebraic operations involving no evaluation/refining of
-- --   forms and for forms in the same vector space
-- checkList :: Int -> [Int -> Property]
-- checkList max = [
--         propT 3 propV_addAssoc,
--         propT 3 propA_wedgeAssoc,
--         propT 2 (const propV_addComm),
--         propT 2 (\_ x y -> forAll c $ \a -> propV_scladdVDistr a x y)
--      ] ++
--      map (\p -> propT 1 (\_ _ v -> forAll (pairOf c c) (\(a,b) -> p a b v)))
--          [ propV_sclTwice, propV_scladdFDistr ]
--   where
--     c = intNumG :: Gen Double
--     propT :: Testable prop => Int -> (Form Double -> Form Double -> Form Double -> prop)
--                            -> Int -> Property
--     propT = calls max

-- -- | Property generation: header for appropriate form generation, all of the
-- --   same degree, to precede the property test
-- calls :: Testable prop =>
--          Int  -- ^ Maximum underlying vector space dimension to test for
--       -> Int  -- ^ Number of forms to generate for the given property
--       -> (Form Double -> Form Double -> Form Double -> prop) -- ^ Boolean property
--       -> Int  -- ^ Dimension of underlying vector space (to be generated)
--       -> Property
-- calls max argFs prop n = p (mod (abs n) max + 1)
--   where p n = forAll (choose (1,n)) $ \k -> propHolds n k !! (argFs - 1)
--         nkForms n k = vectorOf argFs (sized $ kform n k nIntNumG)
--         propHolds n k = [
--           forAll (nkForms n k) (\[x]      -> prop _o _o x),
--           forAll (nkForms n k) (\[x,y]    -> prop _o x  y),
--           forAll (nkForms n k) (\[x,y,z]  -> prop x  y  z) ]
--         _o = undefined


-- | Anticommutativity property
-- TODO: update to new property (propA_wedgeAntiComm)
-- prop_antiComm :: Int -> Property
-- prop_antiComm n = p (2 + abs n `mod` 9)   -- manually limited vectorspace dimension
--   where
--     c   = nIntNumG :: Int -> Gen Double
--     p n = forAll (elements (arityPairs n)) $ \(k,j) ->
--           forAll (pairOf (sized $ kform n k c) (sized $ kform n j c)) $ \(w1, w2) ->
--           forAll (knTupGen (k+j) n) $ \vs ->
--             refine dxV (w1 /\ w2) vs ==  -- =~
--             ((-1) ^ (k * j)) * refine dxV (w2 /\ w1) vs


