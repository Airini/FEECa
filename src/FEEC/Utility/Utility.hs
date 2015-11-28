module FEEC.Utility.Utility( Dimension(..),
                             takeIndices,
                             pairM, pairUp,
                             sumR,
                             sumV,
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

-- | Create a pair from two arguments
pairUp :: a -> b -> (a,b)
pairUp x y = (x,y)

-- | Equivalent to 'sum' for 'Ring' types
sumR :: Ring a => [a] -> a
sumR = foldl add addId

-- | Summation over vector spaces
sumV :: VectorSpace v => [v] -> v
sumV (v:vs) = foldl addV v vs
sumV _ = error "sumV: Need at least one vector to sum! \n"

-- | Numerical equality accounting for round-off errors
eqNum :: Field a => a -> a -> Bool
eqNum a b
    | (a' /= 0.0) && (b' /= 0.0) = toDouble (abs ((a' - b') / max a' b')) < 2e-10
    | otherwise = True
    where a' = toDouble a
          b' = toDouble b

-- | Data type for dimension. Used to generate reasonably small dimensions to
-- | use in testing.
data Dimension = Dimension Int
