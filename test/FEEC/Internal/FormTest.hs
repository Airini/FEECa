module FEEC.Internal.FormTest where

import qualified FEEC.Internal.Vector as V
import FEEC.Internal.VectorTest
import FEEC.Internal.Vector
import FEEC.Internal.Form
import FEEC.Bernstein
import Test.QuickCheck
import FEEC.Internal.Spaces
import Control.Monad (liftM, liftM2)
import FEEC.Utility.Discrete
import FEEC.Utility.Utility (pairM)
import Properties
import Debug.Trace


-- TODO: inner + interior product properties


-- * Instantiated testable properties 

-- | Tests batch of properties
main maxDim = do mapM_ quickCheck (checkList maxDim)
                 quickCheck prop_antiComm


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
prop_antiComm :: Int -> Property
prop_antiComm n = p (mod (abs n) 5 +2)  -- manually limited the vectorspace dimension...
  where c   = nIntNumG :: Int -> Gen Double
        p n = forAll (elements (arityPairs n)) $ \(k,j) ->
              forAll (pairOf (sized $ kform n k c) (sized $ kform n j c)) $ \(df1, df2) ->
              forAll (knTupGen (k+j) n) $ \vs ->
                refine dxV (df1 /\ df2) vs ==  -- =~
                ((-1) ^ (k * j)) * refine dxV (df2 /\ df1) vs


-- * Generating functions and examples

-- | "Integer" coefficients generator
intNumG :: Num f => Gen f
intNumG = liftM fromInteger (arbitrary :: Gen Integer)

nIntNumG :: Num f => Int -> Gen f
nIntNumG = const intNumG

nkIntNumG :: Num f => Int -> Int -> Gen f
nkIntNumG = const nIntNumG

-- | Form generator
kform :: Int  -- ^ n: vectorspace to be applied to dimension
      -> Int  -- ^ k: form arity
      -> (Int -> Gen f)
      -> Int  -- ^ terms: number of terms (constituents)
      -> Gen (Form f)
kform n k coeffg terms = do
  diffs  <- vectorOf (terms+1) (vectorOf k arbitrary)
  coeffs <- vectorOf (terms+1) (coeffg n)
  let capDs = map (map ((+1) . flip mod n)) diffs
  return $ Form k n (zip coeffs capDs)

-- Truncating generator for vectors of 'Double': to avoid errors in computation
-- dependent on order of function application when it comes to floating points
-- OR: small values (overflows and sizing in testing... otherwise size number of terms)
--    Also somewhat dependent on possibility of simplifying forms
nVecGen :: Num f => Int -> Gen (V.Vector f)
nVecGen n = liftM (V.vector) $ -- map (fromIntegral . round)) $
              vectorOf n intNumG--(liftM fromInteger (choose (-11,11::Integer))) -- liftM fromIntegral (arbitrary :: Gen Int)) -- :: Gen Double)


knTupGen :: Num f => Int -> Int -> Gen [V.Vector f]
knTupGen k n = vectorOf k (nVecGen n)

--ffkn :: Ring r => Int -> Int -> Form r
--ffkn n k = undefined
  
-- An example
dd :: Double
omm = Form 1 2 [(1.0,[1])]
umm = Form 1 2 [(1.0,[1])]
uo = omm /\ umm
dd = refine dxV uo [Vector [-3,7], Vector [5,-2]]


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


