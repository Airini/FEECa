module Utility(takeIndices,
               pairM,
               decInd) where

takeIndices :: [a] -> [Int] -> [a]
takeIndices l = map (l !!)

-- | Pair component-wise pair-function application
pairM :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairM f h (x,y) = (f x, h y)

-- | Decrease element in multi-index
decInd :: Int -> [Int] -> [Int]
decInd i a
  | (i >= 0) && (i < length a) = take i a ++ ((max 0 (a!!i)-1) : drop (i+1) a)
  | otherwise = error "decInd: Illegal index"

