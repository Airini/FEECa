{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
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


-- | Tests batch of properties
main maxDim = do mapM_ quickCheck (checkList maxDim)
                 quickCheck prop_antiComm

-- TODO: inner + interior product properties
--       DiffForm tests


-- | Tests for algebraic operations involving no evaluation/refining of
--   forms and for forms in the same vector space
checkList :: Int -> [Int -> Property]
checkList max = [
        propT 3 propV_addAssoc,
        propT 3 propA_wedgeAssoc,
        propT 2 (const propV_addComm),
        propT 2 (\_ x y -> forAll intCofG $ \a -> propV_scladdVDistr a x y)
     ] ++
     map (\p -> propT 1 (\_ _ v -> forAll (pairOf intCofG intCofG) (\(a,b) -> p a b v)))
         [ propV_sclTwice, propV_scladdFDistr ]
  where
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
        nkForms n k = vectorOf argFs (sized $ kform n k)
        propHolds n k = [
          forAll (nkForms n k) (\[x]      -> prop _o _o x),
          forAll (nkForms n k) (\[x,y]    -> prop _o x  y),
          forAll (nkForms n k) (\[x,y,z]  -> prop x  y  z) ]
        _o = undefined

dd :: Double
omm = Form 1 2 [(1.0,[1])]
umm = Form 1 2 [(1.0,[1])]
uo = omm /\ umm
dd = refine dxV uo [Vector [-3,7], Vector [5,-2]]

-- | Anticommutativity property
-- TODO: update to new property (propA_wedgeAntiComm)
prop_antiComm :: Int -> Property
prop_antiComm n = p (mod (abs n) 5 +2)  -- manually limited the vectorspace dimension...
  where p n = forAll (elements (arityPairs n)) $ \(k,j) ->
              forAll (pairOf (sized $ kform n k) (sized $ kform n j)) $ \(df1, df2) ->
              forAll (knTupGen (k+j) n) $ \(Tp vs) ->
                refine dxV (df1 /\ df2) vs ==  -- =~
                ((-1) ^ (k * j)) * refine dxV (df2 /\ df1) vs

-- | "Integer" coefficients generator
intCofG :: Gen Double
intCofG = liftM fromInteger (arbitrary :: Gen Integer)

-- | Form generator
kform :: Int  -- ^ n: vectorspace to be applied to dimension
      -> Int  -- ^ k: form arity
      -> Int  -- ^ terms: number of terms (constituents)
      -> Gen (Form Double)
kform n k terms = do
  diffs  <- vectorOf (terms+1) (vectorOf k arbitrary)
  coeffs <- vectorOf (terms+1) (liftM fromIntegral (arbitrary :: Gen Int))
  let capDs = map (map ((+1) . flip mod n)) diffs
  return $ Form k n (zip coeffs capDs)
{-
instance Arbitrary (Vector Double) where
  arbitrary = sized nVecGen
-}

-- Truncating generator for vectors of 'Double': to avoid errors in computation
-- dependent on order of function application when it comes to floating points
-- OR: small values (overflows and sizing in testing... otherwise size number of terms)
--    Also somewhat dependent on possibility of simplifying forms
nVecGen :: Int -> Gen (V.Vector Double)
nVecGen n = liftM (V.vector) $ -- map (fromIntegral . round)) $
                         vectorOf n (liftM fromInteger (choose (-11,11::Integer))) -- liftM fromIntegral (arbitrary :: Gen Int)) -- :: Gen Double)

-- Tuples of vectors (for form argument generation)
newtype Tuple = Tp [V.Vector Double]

instance Show Tuple where
  show (Tp xs) = show xs

knTupGen :: Int -> Int -> Gen Tuple
knTupGen k n = liftM Tp $ vectorOf k (nVecGen n)


-- * Basic example implementation for generic vectors (coordinates with
--   respect to a basis)

-- data Vector f = Vex Dim [f]

-- | Vector invariant: the number of components is valid
-- vectorInvariant (Vex n xs) = n == length xs
{-
instance Show f => Show (Vector f) where
  show (Vex n xs) = show n ++ "-vector " ++ show xs

addList :: Ring f => Vector f -> Vector f -> Vector f
addList (Vex n xs) (Vex m ys)
  | n /= m = error "addList: vectors must belong to the same space"
  | otherwise = Vex n (zipWith add xs ys)

scaleList :: Ring f => f -> Vector f -> Vector f
scaleList a (Vex n xs) = Vex n (map (mul a) xs)

instance Ring f => VectorSpace (Vector f) where
  type Scalar (Vector f) = f
  addV = addList
  sclV = scaleList
-}

-- | Our basic projection for 'Vector f': usual 1-form basis == external
--   derivative of global coordinate functions
dxV :: Int -> V.Vector f -> f
dxV i (V.Vector x) = x !! (i-1)

{-
instance Field Int where
  add = (+)
  addId = 0
  addInv = (0-)
  mul = (*)
  mulId = 1
  mulInv = div 1
-}

-- To toggle testing between Int and rounded Double
type Cof = Double

-- Truncating generator for vectors of 'Double': to avoid errors in computation
-- dependent on order of function application when it comes to floating points
-- instance Arbitrary (Vector Double) where
--  arbitrary = liftM (Vex 4 . map (fromIntegral . round)) $
--                           vectorOf 4 (arbitrary :: Gen Double)

-- For the very basic test: fixed size of wedge-resultant arity
newtype Tup4 = V4 [V.Vector Double]
  deriving Show

instance Arbitrary Tup4 where
  arbitrary = liftM V4 $ vectorOf 4 (nVecGen 4) -- arbitrary

instance Arbitrary (Form Double) where
  arbitrary = sized (kform 4 2)

-- | Very basic test: fixed vectorspace dimensions and form arity
prop_first :: Form Double -> Form Double -> Tup4 -> Bool
prop_first df1 df2 (V4 vs) =
    refine dxV (df1 /\ df2) vs ==
    ((-1) ^ (arity df1 * arity df2)) * refine dxV (df2 /\ df1) vs


-- * Helper functions

-- | Pair generator from two generators
pairOf :: Gen a -> Gen b -> Gen (a,b)
pairOf = liftM2 (,)

-- | 'Cof' threshold for errors
threshold :: Double
threshold = 1e-15

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



