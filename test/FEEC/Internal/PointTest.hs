module FEEC.Internal.PointTest where

import FEEC.Internal.Spaces
import FEEC.Internal.Vector
import FEEC.Internal.Point
import FEEC.Utility.Utility
import Test.QuickCheck (Arbitrary(..), choose, Property, quickCheck)
import qualified Test.QuickCheck as Q(vector)

-- | Type to represent a point in the n-dimensional unit cube.
newtype UnitCube = UnitCube Point deriving (Show, Eq)

-- | Randomly pick a dimension n and a point from the n-dimensional unit
-- | cube.
instance Arbitrary UnitCube where
    arbitrary = do n <- choose (1,10)
                   cs <- Q.vector n
                   let transf l = zipWith (-) l (map (fromInt . truncate) l)
                   return (UnitCube (point (map abs (transf cs))))

prop_cubicToBarycentric :: UnitCube -> Bool
prop_cubicToBarycentric (UnitCube p) =
    (prop_barycentric_range p') && (prop_barycentric_sum p') && (dim p' == dim p + 1)
        where p' = cubicToBarycentric p

prop_barycentric_range :: Point -> Bool
prop_barycentric_range p = (all (0 <=) cs) && (all (1 >=) cs)
    where cs = components (fromPoint p)

prop_barycentric_sum :: Point -> Bool
prop_barycentric_sum p = eqNum (sum cs) 1
    where cs = components (fromPoint p)
