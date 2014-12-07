{-# LANGUAGE FlexibleInstances #-}
module FormsTest where

import Forms
import Test.QuickCheck
import Spaces
import Control.Monad (liftM)

instance Arbitrary (Form (Vector Int) Int) where
  arbitrary = sized (kform 4 2)

instance Show (Form (Vector Int) Int) where
  show (Fform k cs _) = show k ++ "-form: " ++ show cs

instance Show (Vector Int) where
  show (Vex n xs) = show n ++ "-vector: " ++ show xs

instance Field Double where
  add = (+)
  addId = 0
  addInv = (0-)
  mul = (*)
  mulId = 1
  mulInv = recip

instance Field Int where
  add = (+)
  addId = 0
  addInv = (0-)
  mul = (*)
  mulId = 1
  mulInv = div 1

kform :: Int -> Int -> Int -> Gen (Form (Vector Int) Int)
kform n k terms = do
  diffs <- vectorOf terms (vectorOf k arbitrary)
  coeffs <- vectorOf terms arbitrary
  let capDs = map (map (flip mod n)) diffs
  return $ Fform k (zip capDs coeffs) undefined

instance Arbitrary (Vector Int) where
  arbitrary = liftM (Vex 4) $ vectorOf 4 arbitrary

newtype Tup4 = V4 [Vector Int]
  deriving Show

instance Arbitrary Tup4 where
  arbitrary = liftM V4 $ vectorOf 4 arbitrary

prop_first :: Form (Vector Int) Int
            -> Form (Vector Int) Int
            -> Tup4 -> Bool --  [Vector Double] -> Bool
prop_first df1 df2 (V4 vs) = (operator $ refine dxV (df1 //\\ df2)) vs -- (map (\(Vex _ xs) -> xs) vs)
                      == ((-1) ^ (arity df1 + arity df2)) * ((operator $ refine dxV (df2 //\\ df1)) vs)


