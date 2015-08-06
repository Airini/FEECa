module FEEC.Utility.Utility( Dimension(..),
                             takeIndices,
                             pairM,
                             eqNum,
                             toDouble,
                             fromDouble
                           ) where

import Test.QuickCheck

takeIndices :: [a] -> [Int] -> [a]
takeIndices l = map (l !!)

-- | Pair component-wise pair-function application
pairM :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairM f h (x,y) = (f x, h y)

-- | Numerical equality accounting for round-off errors
eqNum :: RealFloat a => a -> a -> Bool
eqNum a b
    | (a /= 0) && (b /= 0) = abs ((a - b) / max a b) < 2e-9
    | otherwise = True

-- | Data type for dimension. Used to generate reasonably small dimensions to
-- | use in testing.
data Dimension = Dimension Int

instance Arbitrary Dimension where
    arbitrary = do
      n <- choose(0, 10)
      return (Dimension n)

toDouble :: RealFrac a => [a] -> [Double]
toDouble = map realToFrac

fromDouble :: RealFrac a => [Double] -> [a]
fromDouble = map realToFrac
