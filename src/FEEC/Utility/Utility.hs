module FEEC.Utility.Utility( Dimension(..),
                             takeIndices,
                             pairM,
                             eqNum,
                             toDouble,
                             fromDouble
                           ) where

--import Test.QuickCheck
import FEEC.Internal.Spaces

takeIndices :: [a] -> [Int] -> [a]
takeIndices l = map (l !!)

-- | Pair component-wise pair-function application
pairM :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairM f h (x,y) = (f x, h y)

-- | Numerical equality accounting for round-off errors
eqNum :: Field a => a -> a -> Bool
eqNum a b
    | (a' /= 0.0) && (b' /= 0.0) = toDouble (abs ((a' - b') / (max a' b'))) < 2e-10
    | otherwise = True
    where a' = toDouble a
          b' = toDouble b

-- | Data type for dimension. Used to generate reasonably small dimensions to
-- | use in testing.
data Dimension = Dimension Int
