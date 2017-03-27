{-# LANGUAGE CPP #-}

module FEECa.Utility.Utility (
    Dimension (..)
  , sortOn, takeIndices
  , pairM, pairUp, zipWithWhen
  , sumR, productR, sumV
  , expSign, sign
  , eqNum
  , toDouble, fromDouble
  ) where

import FEECa.Internal.Spaces

#if MIN_VERSION_base(4,8,0)
import Data.List              ( sortOn )
#else
import Prelude        hiding  ( foldr, sum )
import Data.Foldable          ( Foldable (..), foldr, sum )
import Data.List              ( sortBy )
import Data.Ord               ( comparing )

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd .
            sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
#endif

takeIndices :: [a] -> [Int] -> [a]
takeIndices l = map (l !!)

-- | Pair component-wise pair-function application
pairM :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairM f h (x,y) = (f x, h y)

-- | Create a pair from two arguments
pairUp :: a -> b -> (a,b)
pairUp x y = (x,y)

-- | 'zipWith' pairs from parallel lists which fulfill some property.
zipWithWhen :: (a -> b -> c) -> (a -> b -> Bool) -> [a] -> [b] -> [c]
zipWithWhen _ _ []    _       = []
zipWithWhen _ _ _     []      = []
zipWithWhen f p (a:as) (b:bs) | p a b     = f a b : zipWithWhen f p as bs
                              | otherwise = zipWithWhen f p as bs

-- | Equivalent to 'sum' for 'Ring' types
sumR :: (Foldable t, Ring a) => t a -> a
sumR = foldr add addId

-- | Equivalent to 'product' for 'Ring' types
productR :: (Foldable t, Ring a) => t a -> a
productR = foldr mul mulId

{-# INLINE pairM #-}
{-# INLINE pairUp #-}
{-# INLINE sumR #-}
{-# INLINE productR #-}
{-# INLINE sumV #-}

-- | Returns the appropriate function for arithmetic expressions having
--   products with exponentials of -1 over 'Ring' types
expSign :: Ring f => Int -> f -> f
expSign i | mod (i+1) 2 == 0 = id
          | otherwise        = mul (addInv mulId)

-- | Sign of a permutation defined by a pair of increasing permutations:
--   specialised to an appropriate 'Ring' type (required for operations)
sign :: Ring f => ([Int], [Int]) -> f
sign (p1, p2) = if sum [ length (filter (i <) p1) | i <- p2 ] `mod` 2 == 0
                  then mulId
                  else addInv mulId

-- | Summation over modules.
sumV :: Module m => [m] -> m
sumV (v:vs) = foldr addV v vs
sumV _      = error "sumV: Need at least one vector to sum!\n"

-- | Numerical equality accounting for round-off errors
eqNum :: Field a => a -> a -> Bool
eqNum a b
    | (a' /= 0.0) && (b' /= 0.0)  = toDouble (abs ((a' - b') / max a' b')) < eps
    | otherwise                   = max (abs a') (abs b') < eps
  where a'  = toDouble a
        b'  = toDouble b
        eps = 2e-10

-- | Data type for dimension. Used to generate reasonably small dimensions to
-- | use in testing.
data Dimension = Dimension Int
