{-# LANGUAGE FlexibleInstances #-}
module FormsTest where

import Forms
import Test.QuickCheck
import Spaces
import Control.Monad (liftM, liftM2)
import Discrete

instance Field Double where
  add = (+)
  addId = 0
  addInv = (0-)
  mul = (*)
  mulId = 1
  mulInv = recip

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
newtype Tup4 = V4 [Vector Cof]
  deriving Show

instance Arbitrary Tup4 where
  arbitrary = liftM V4 $ vectorOf 4 (nVecGen 4) -- arbitrary

instance Arbitrary (Form Cof) where
  arbitrary = sized (kform 4 2)

-- | Very basic test: fixed vectorspace dimensions and form arity
prop_first :: Form Cof -> Form Cof -> Tup4 -> Bool
prop_first df1 df2 (V4 vs) =
    (refine dxV (df1 /\ df2)) vs ==
    ((-1) ^ (arity df1 * arity df2)) * ((refine dxV (df2 /\ df1)) vs)


-- | Form generator
kform :: Int  -- ^ n: vectorspace to be applied to dimension
      -> Int  -- ^ k: form arity
      -> Int  -- ^ terms: number of terms (constituents)
      -> Gen (Form Cof)
kform n k terms = do
  diffs  <- vectorOf terms (vectorOf k arbitrary)
  coeffs <- vectorOf terms (liftM fromIntegral (arbitrary :: Gen Int))
  let capDs = map (map ((+1) . flip mod n)) diffs
  return $ Fform k (zip coeffs capDs)

instance Arbitrary (Vector Double) where
  arbitrary = sized nVecGen

-- Truncating generator for vectors of 'Double': to avoid errors in computation
-- dependent on order of function application when it comes to floating points
-- OR: small values (overflows and sizing in testing... otherwise size number of terms)
--    Also somewhat dependent on possibility of simplifying forms
nVecGen :: Int -> Gen (Vector Cof)
nVecGen n = liftM (Vex n) $ -- map (fromIntegral . round)) $
                         vectorOf n (return 0.1) -- liftM fromIntegral (arbitrary :: Gen Int)) -- :: Gen Double)

-- Tuples of vectors (for form argument generation)
newtype Tuple = Tp [Vector Cof]

instance Show Tuple where
  show (Tp xs) = show xs

knTupGen :: Int -> Int -> Gen Tuple
knTupGen k n = liftM Tp $ vectorOf k (nVecGen n)

-- | Anticommutativity property
-- TODO: extract appropriately dimensioned generation heading as a driver for
-- testing for different form evaluation properties
prop_ :: Int -> Property
prop_ n = p ((mod (abs n) 5) +2)  -- manually limited the vectorspace dimension...
  where p n = forAll (elements (arityPairs n)) $ \(k,j) ->
              forAll (pairOf (sized $ kform n k) (sized $ kform n j)) $ \(df1, df2) ->
              forAll (knTupGen (k+j) n) $ \(Tp vs) ->
                ((refine dxV (df1 /\ df2)) vs) ==  -- =~
                (((-1) ^ (k * j)) * ((refine dxV (df2 /\ df1)) vs))


-- * Helper functions

-- | Pair generator from two generators
pairOf :: Gen a -> Gen b -> Gen (a,b)
pairOf = liftM2 (,)

-- | 'Cof' threshold for errors
threshold :: Cof
threshold = 1e-15

-- | Approximate equality for 'Cof': error within 'threshold'
(=~) :: Cof -> Cof -> Bool
x =~ y = (abs (x-y)) < threshold

-- # Wikipedia
machEsp :: RealFrac a => a
machEsp = until p (/2) 1
  where p eps = eps/2 + 1 == 1
   
main :: IO ()
main = do
     putStrLn "Calculated machine epsilon:"
     putStrLn ("  Float: " ++ show (machEsp :: Float))
     putStrLn ("  Double: " ++ show (machEsp :: Double))
-- # end Wikipedia
---


-- Static examples: some 'QuickCheck' generated, ie: not likeable to the eye

v1 :: Vector Double
v1 = Vex 3 [1.0, 2.0, 3.0]
v2 :: Vector Double
v2 = Vex 3 [1.2, 2.5, 1.0]
ex1 = Fform 1 [(2, [1])]

a1 = refine dxV ex1 [v1]
a2 = refine dxV ex2 [v1]
a3 = refine dxV (ex1 /\ ex2) [v1, v2]
a4 = refine dxV (ex2 /\ ex1) [v1, v2]

ex2 = Fform 1 [(3, [2])]

swapP :: (a,b) -> (b,a)
swapP (x,y) = (y,x)

t1 = Fform 1 [(2.0,[2]),(-47.0,[0]),(-35.0,[2]),(-50.0,[1]),(-3.0,[1]),(29.0,[0]),(-11.0,[1]),(-17.0,[1]),(-6.0,[0]),(30.0,[1])]
t2 = Fform 1 $ map swapP [([2],-17.0),([2],53.0),([0],-36.0),([1],-51.0),([2],-47.0),([1],-28.0),([0],58.0),([0],-48.0),([0],-4.0),([1],20.0)]
b1 = Vex 3 [723.0,255.0,-109.0]
b2 = Vex 3 [-340.0,-1018.0,297.0]
aux1 d1 d2 vs = refine dxV (d1 /\ d2) vs

t3 = Fform 2 $ map swapP[([2,1],813.0),([1,0],351.0),([1,2],903.0),([3,1],816.0),([2,0],180.0),([0,0],314.0),([0,3],373.0),([0,1],-988.0),([0,3],-284.0),([1,3],301.0),([1,3],-161.0),([0,0],842.0),([0,2],407.0),([1,3],-959.0),([1,3],954.0),([0,1],639.0)]
t4 = Fform 2 $ map swapP[([2,1],981.0),([3,0],150.0),([1,0],692.0),([2,1],674.0),([3,0],-354.0),([3,3],927.0),([1,3],-869.0),([0,3],238.0),([3,1],575.0),([0,3],433.0),([2,0],359.0),([2,1],554.0),([2,1],259.0),([2,3],16.0),([3,0],923.0),([3,3],936.0)]

b3 = Vex 4 [208.0,770.0,-278.0,189.0]
b4 = Vex 4 [601.0,862.0,989.0,-212.0]
b5 = Vex 4 [694.0,669.0,1014.0,-303.0]
b6 = Vex 4 [74.0,268.0,-963.0,-577.0]


