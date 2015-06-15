module FEEC.Utility.Utility( Dimension(..),
                             takeIndices,
                             pairM,
                             eqNum ) where

import Test.QuickCheck

takeIndices :: [a] -> [Int] -> [a]
takeIndices l = map (l !!)

-- | Pair component-wise pair-function application
pairM :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairM f h (x,y) = (f x, h y)

-- | Numerical equality accounting for round-off errors
eqNum :: Double -> Double -> Bool
eqNum a b = abs (a - b) < 2e-11

-- | Data type for dimension. Used to generate reasonably small dimensions to
-- | use in testing.
data Dimension = Dimension Int

instance Arbitrary Dimension where
    arbitrary = do
      n <- choose(0, 10)
      return (Dimension n)
