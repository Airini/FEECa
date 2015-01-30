module SimplexTest where
import qualified Math.Combinatorics.Exact.Binomial as C

import Data.Maybe
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import qualified Test.QuickCheck.Property as QCP
import Spaces
import Simplex
import System.Random

data SubsimplexTest v = SubsimplexTest (Simplex v) Int Int

instance Arbitrary v => Arbitrary (SubsimplexTest v) where
    arbitrary = do
      n <- choose(0,10)
      l <- vector (n+1)
      k <- choose(0,n)
      i <- choose(0,max ((n+1) `C.choose` (k+1)-1) 0)
      return (SubsimplexTest (Simplex l) k i)

instance Show (SubsimplexTest v) where
    show (SubsimplexTest l k i) = "Supersimplex n=" ++
                                  show (length (vertices l) - 1)
                                  ++ ", k=" ++ show k ++ ", i=" ++ show i

instance Arbitrary v => Arbitrary (Simplex v) where
    arbitrary = do
      l <- arbitrary
      return (Simplex l)

-- A subsimplex should contain only vertices contained in the supersimplex,
-- and the length should be its topological dimension + 1.
prop_subsimplex :: (Eq v, Show v) => SubsimplexTest v -> Bool
prop_subsimplex (SubsimplexTest s@(Simplex l) k i) =
    (length subl == k+1) && all (`elem` l) subl
    where n         = topologicalDimension s
          subs      = subsimplex s k i
          subl      = vertices subs
          indexList = map (\x -> fromJust (elemIndex x l)) subl

-- subsimplices should return (n+1) choose (k+1) subsimplices and the i:th
-- subsimplex should be the same as subsimplex k i
prop_subsimplices :: Eq v => SubsimplexTest v -> Bool
prop_subsimplices (SubsimplexTest s@(Simplex l) k _) =
    (length subs == m) && all ithSubsimplexVertices [0..m-1]
    where n    = topologicalDimension s
          m    = (n+1) `C.choose` (k+1)
          subs = subsimplices s k
          ithSubsimplexVertices i =
              vertices (subs!!i) ==  vertices (subsimplex s k i)

main = do quickCheck (prop_subsimplex :: SubsimplexTest [Int] -> Bool)
          quickCheck (prop_subsimplices :: SubsimplexTest [Int] -> Bool)
