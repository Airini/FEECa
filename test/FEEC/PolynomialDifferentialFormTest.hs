module FEEC.PolynomialDifferentialFormTest where

import Data.Maybe

import FEEC.Bernstein
import FEEC.Internal.Form
import FEEC.Internal.Simplex
import FEEC.Internal.SimplexTest
import FEEC.Internal.Spaces
import FEEC.Internal.Vector
import FEEC.PolynomialDifferentialForm( DifferentialForm, findSimplex, apply )
import qualified FEEC.PolynomialDifferentialForm as D
import FEEC.Utility.Combinatorics
import FEEC.Utility.Print
import FEEC.Utility.Utility

import Test.QuickCheck(Arbitrary, arbitrary, quickCheck, (==>), Property)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as Q

--------------------------------------------------------------------------------
-- Random Simplices
--------------------------------------------------------------------------------

n = 3

test :: IO (Simplex (Vector Double))
test = Q.generate (arbitrarySimplex n)


t :: Simplex (Vector Double)
t = simplex [vector [0], vector [12]]

vs = spanningVectors t

omega :: Simplex (Vector Double) -> DifferentialForm Double
omega t = Form n n [(lambda 1, [1..n])]
    where lambda i = barycentricCoordinate t i
          n = topologicalDimension t

-- | Check that lambda_1 /\ ... /\ lambda_n ( v_1, ..., v_n) == 1.0 as stated
-- | on p. 44 in Arnold, Falk, Winther.
prop_volume_form :: Simplex (Vector Double) -> (Vector Double) -> Property
prop_volume_form t v = volume t > 0 ==> evaluate v b `eqNum` 1.0
    where b = apply omega vs
          vs = spanningVectors t
          omega = Form n n [(redefine t mulId, [1..n])]
          n = topologicalDimension t

prop_integral :: Simplex (Vector Double) -> Property
prop_integral t = volume t > 0
                  ==> all (nfac `eqNum`) [D.integrate t (omega i) | i <- [1..n]]
    where omega i  = Form n n [(lambda i, [1..n])]
          lambda i = barycentricCoordinate t i
          nfac     = 1.0 / (factorial (n + 1))
          n        = topologicalDimension t

