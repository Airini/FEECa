module FEECa.Utility.Utility(
    Dimension (..)
  , takeIndices
  , pairM, pairUp
  , sumR, sumV
  , expSign, sign
  , eqNum
  , toDouble
  ) where

import FEECa.Internal.Spaces

takeIndices :: [a] -> [Int] -> [a]
takeIndices l = map (l !!)

-- | Pair component-wise pair-function application
pairM :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairM f h (x,y) = (f x, h y)

-- | Create a pair from two arguments
pairUp :: a -> b -> (a,b)
pairUp x y = (x,y)

-- | Equivalent to 'sum' for 'Ring' types
sumR :: Ring a => [a] -> a
sumR = foldl add addId

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

-- | Summation over vector spaces
sumV :: VectorSpace v => [v] -> v
sumV (v:vs) = foldl addV v vs
sumV _      = error "sumV: Need at least one vector to sum!\n"

-- | Numerical equality accounting for round-off errors
eqNum :: Field a => a -> a -> Bool
eqNum a b
    | (a' /= 0.0) && (b' /= 0.0)  = toDouble (abs ((a' - b') / max a' b')) < 2e-10
    | otherwise                   = max (abs a') (abs b') < 2e-10
  where a' = toDouble a
        b' = toDouble b

-- | Data type for dimension. Used to generate reasonably small dimensions to
-- | use in testing.
data Dimension = Dimension Int
