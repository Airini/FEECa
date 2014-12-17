{-# LANGUAGE FlexibleInstances #-}
module FormsTest where

import Forms
import Test.QuickCheck
import Spaces
import Control.Monad (liftM, liftM2)
import Discrete

{-
instance Field Double where
  add = (+)
  addId = 0
  addInv = (0-)
  mul = (*)
  mulId = 1
  mulInv = recip
-}

instance Field Int where
  add = (+)
  addId = 0
  addInv = (0-)
  mul = (*)
  mulId = 1
  mulInv = div 1

-- To toggle testing between Int and rounded Double
type Cof = Double

kform :: Int -> Int -> Int -> Gen (Form Cof)
kform n k terms = do
  diffs  <- vectorOf terms (vectorOf k arbitrary)
  coeffs <- vectorOf terms (liftM fromIntegral (arbitrary :: Gen Int))
  let capDs = map (map (flip mod n)) diffs
  return $ Fform k (zip capDs coeffs)

-- Truncating generator for vectors of 'Double': to avoid errors in computation
-- dependent on order of function application when it comes to floating points
--instance Arbitrary (Vector Double) where
--  arbitrary = liftM (Vex 4 . map (fromIntegral . round)) $
--                           vectorOf 4 (arbitrary :: Gen Double)

instance Arbitrary (Form Cof) where
  arbitrary = sized (kform 4 2)

newtype Tup4 = V4 [Vector Cof]
  deriving Show

instance Arbitrary Tup4 where
  arbitrary = liftM V4 $ vectorOf 4 arbitrary

prop_first :: Form Cof
            -> Form Cof
            -> Tup4 -> Bool
prop_first df1 df2 (V4 vs) =
    (refine dxV (df1 //\\ df2)) vs ==
    ((-1) ^ (arity df1 * arity df2)) * ((refine dxV (df2 //\\ df1)) vs)

pairOf :: Gen a -> Gen b -> Gen (a,b)
pairOf = liftM2 (,)

instance Arbitrary (Vector Double) where
  arbitrary = sized nVecGen

-- Truncating generator for vectors of 'Double': to avoid errors in computation
-- dependent on order of function application when it comes to floating points
nVecGen :: Int -> Gen (Vector Cof)
nVecGen n = liftM (Vex n) $ -- map (fromIntegral . round)) $
                         vectorOf n (liftM fromIntegral (arbitrary :: Gen Int)) -- :: Gen Double)

newtype Tuple = Tp [Vector Cof]

instance Show Tuple where
  show (Tp xs) = show xs

knTupGen :: Int -> Int -> Gen Tuple
knTupGen k n = liftM Tp $ vectorOf k (nVecGen n)

prop_ :: Int -> Property
prop_ n = p ((mod (abs n) 5)+2)
  where p n = forAll (elements (arityPairs n)) $ \(k,j) ->
              forAll (pairOf (sized $ kform n k) (sized $ kform n j)) $ \(df1, df2) ->
              forAll (knTupGen (k+j) n) $ \(Tp vs) ->
                (refine dxV (df1 //\\ df2)) vs ==
                ((-1) ^ (k * j)) * ((refine dxV (df2 //\\ df1)) vs)


