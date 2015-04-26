module Utility(takeIndices,
               pairM) where

takeIndices :: [a] -> [Int] -> [a]
takeIndices l = map (l !!)

-- | Pair component-wise pair-function application
pairM :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairM f h (x,y) = (f x, h y)

-- | Evaluates the legendre polynomial of degree n at x.
legendre :: Integral a => a -> Double -> Double
legendre n x
    | n == 0 = 1
    | n > 0 = legendre' n 1 x 1.0 x

legendre' :: Integral a => a -> a -> Double -> Double -> Double -> Double
legendre' n k x pk1 pk
    | n == 1 = pk
    | otherwise = legendre' (n - 1) (k + 1) x lpk1' pk'
    where lpk1' = pk
          pk' = (((2 * k' + 1) * x * pk) - k' * pk1) / (k' + 1)
          k' = fromIntegral k
