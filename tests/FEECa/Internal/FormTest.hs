module FEECa.Internal.FormTest where

import Control.Monad  ( liftM, liftM2 )
import Data.List      ( nub, deleteBy )

import qualified FEECa.Internal.Vector as V
import FEECa.Internal.Form
import FEECa.Internal.Spaces
import FEECa.Bernstein

import FEECa.Utility.Discrete
import FEECa.Utility.Utility ( pairM, sumV )

import FEECa.Internal.VectorTest

import Properties
import Test.QuickCheck
import Test.QuickCheck.Test ( isSuccess )

-- TODO: inner + interior product properties


-- * Instantiated testable properties 

-- | Tests batch of properties
testForm maxDim = do
  putStrLn $
    "Testing alternating forms of vector space dimension up to " ++ show maxDim
  ps <- mapM quickCheckResult (checkList maxDim)
  ap <- quickCheckResult (label "Anticommutativity" $ prop_antiComm maxDim)
  return $ all isSuccess (ap : ps)

-- | Tests for algebraic operations involving no evaluation/refining of
--   forms and for forms in the same vector space
checkList :: Int -> [Int -> Property]
checkList max = [
        propT 3 propV_addAssoc,
        propT 3 propA_wedgeAssoc,
        propT 2 (const propV_addComm),
        propT 2 (\_ x y -> forAll c $ \a -> propV_scladdVDistr a x y)
     ] ++
     map (\p -> propT 1 (\_ _ v -> forAll (pairOf c c) (\(a,b) -> p a b v)))
         [ propV_sclTwice, propV_scladdFDistr ]
  where
    c = intNumG :: Gen Double
    propT :: Testable prop => Int -> (Form Double -> Form Double -> Form Double -> prop)
                           -> Int -> Property
    propT = calls max

-- | Property generation: header for appropriate form generation, all of the
--   same degree, to precede the property test
calls :: Testable prop =>
         Int  -- ^ Maximum underlying vector space dimension to test for
      -> Int  -- ^ Number of forms to generate for the given property
      -> (Form Double -> Form Double -> Form Double -> prop) -- ^ Boolean property
      -> Int  -- ^ Dimension of underlying vector space (to be generated)
      -> Property
calls max argFs prop n = p (mod (abs n) max + 1)
  where p n = forAll (choose (1,n)) $ \k -> propHolds n k !! (argFs - 1)
        nkForms n k = vectorOf argFs (sized $ kform n k nIntNumG)
        propHolds n k = [
          forAll (nkForms n k) (\[x]      -> prop _o _o x),
          forAll (nkForms n k) (\[x,y]    -> prop _o x  y),
          forAll (nkForms n k) (\[x,y,z]  -> prop x  y  z) ]
        _o = undefined


-- | Anticommutativity property
-- TODO: update to new property (propA_wedgeAntiComm)
prop_antiComm :: Int -> Int -> Property
prop_antiComm max n = p (2 + abs n `mod` max)   -- manually limited module dimension
  where
    c   = nIntNumG :: Int -> Gen Double
    p n = forAll (elements (arityPairs n)) $ \(k,j) ->
          forAll (pairOf (sized $ kform n k c) (sized $ kform n j c)) $ \(w1, w2) ->
          forAll (knTupGen (k+j) n) $ \vs ->
            refine dxV (w1 /\ w2) vs ==  -- =~
            ((-1) ^ (k * j)) * refine dxV (w2 /\ w1) vs


-- * Generating functions and examples

-- | "Integer" coefficients generator
intNumG :: Ring f => Gen f
intNumG = liftM (embedIntegral . getNonZero) (arbitrary :: Gen (NonZero Integer))

nIntNumG :: Ring f => Int -> Gen f
nIntNumG = const intNumG

nkIntNumG :: Ring f => Int -> Int -> Gen f
nkIntNumG = const nIntNumG

prop_genf n k s = forAll (kform n' k' (nIntNumG :: Int -> Gen Double) s) $
                    \w -> prop_invar w
  where n' = (1+) $ n `mod` 5
        k' = 1 + (k `mod` n')

prop_invar (Form _ _ ts) = nub iss == iss && all ((/= addId) . fst) ts
  where iss = map snd ts

prop_anti :: Int -> Property
prop_anti n = p (2 + abs n `mod` 9)
  where c   = nIntNumG :: Int -> Gen Double
        p n = forAll (elements (arityPairs n)) $ \(k,j) ->
              forAll (pairOf (sized $ kform n k c) (sized $ kform n j c)) $
                \(w1, w2) -> prop_invar (w1 /\ w2) && prop_invar (w2 /\ w1)


-- | Form generator
kform :: Ring f
      => Int  -- ^ n: vectorspace to be applied to dimension
      -> Int  -- ^ k: form arity
      -> (Int -> Gen f)
      -> Int  -- ^ terms: number of terms (constituents)
      -> Gen (Form f)
kform n k coeffg terms = do
  diffs  <- vectorOf (terms+1) (vectorOf k (choose (1,n)))
  coeffs <- vectorOf (terms+1) (coeffg n)
  return . sumV $ zipWith (\c is -> Form k n [(c,is)]) coeffs diffs
  {-foldl (\w t -> addV w (Form k n [t])) (zeroForm k n) $-}
  {-Form k n (zip coeffs (take (length coeffs) diffs)) -} 
  --zipWith (\c is -> sclV c (Form k n [(mulId, is)])) coeffs capDs

-- Truncating generator for vectors of 'Double': to avoid errors in computation
-- dependent on order of function application when it comes to floating points
-- OR: small values (overflows and sizing in testing... otherwise size number of terms)
--    Also somewhat dependent on possibility of simplifying forms
nVecGen :: Ring f => Int -> Gen (V.Vector f)
nVecGen n = liftM V.vector $ -- map (fromIntegral . round)) $
              vectorOf n intNumG--(liftM fromInteger (choose (-11,11::Integer))) -- liftM fromIntegral (arbitrary :: Gen Int)) -- :: Gen Double)


knTupGen :: Ring f => Int -> Int -> Gen [V.Vector f]
knTupGen k n = vectorOf k (nVecGen n)


-- An example
dd :: Double
omm = Form 1 2 [(1.0,[1])]
umm = Form 1 2 [(1.0,[1])]
uo = omm /\ umm
dd = refine dxV uo [V.Vector [-3,7], V.Vector [5,-2]]


-- | Our basic projection for 'Vector f': usual 1-form basis == external
--   derivative of global coordinate functions
dxV :: Int -> V.Vector f -> f
dxV i (V.Vector x) = x !! (i-1)

-- * Helper functions

-- | Pair generator from two generators
pairOf :: Gen a -> Gen b -> Gen (a,b)
pairOf = liftM2 (,)

-- | 'Cof' threshold for errors
threshold :: Double
threshold = 1e-10

-- | Approximate equality for 'Cof': error within 'threshold'
(=~) :: Double -> Double -> Bool
x =~ y = abs (x-y) < threshold


-- # Wikipedia
machEsp :: RealFrac a => a
machEsp = until p (/2) 1
  where p eps = eps/2 + 1 == 1

exeEsp :: IO ()
exeEsp = do
     putStrLn "Calculated machine epsilon:"
     putStrLn ("  Float: " ++ show (machEsp :: Float))
     putStrLn ("  Double: " ++ show (machEsp :: Double))
-- # end Wikipedia
---

