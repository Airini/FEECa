module Utility( takeIndices,
                pairM,
                eqNum ) where

takeIndices :: [a] -> [Int] -> [a]
takeIndices l = map (l !!)

-- | Pair component-wise pair-function application
pairM :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairM f h (x,y) = (f x, h y)

-- | Numerical equality accounting for round-off errors
eqNum :: Double -> Double -> Bool
eqNum a b = abs (a - b) < 2e-11
