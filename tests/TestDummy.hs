module Main where

import qualified FEECa.Internal.FormTest        as F
import qualified FEECa.Internal.MultiIndexTest  as MI
import qualified FEECa.Internal.SimplexTest     as S
import qualified FEECa.Internal.VectorTest      as V

import qualified FEECa.Utility.CombinatoricsTest  as C
import qualified FEECa.Utility.QuadratureTest     as Q

import qualified FEECa.BernsteinTest      as B
import qualified FEECa.FiniteElementTest  as FE
import qualified FEECa.PolynomialTest     as P
import qualified FEECa.PolynomialDifferentialFormTest as DF

import Control.Monad    ( liftM, unless )
import System.Exit      ( exitFailure )

import Test.QuickCheck  ( stdArgs, Args(..) )

nmax :: Int
nmax = 4
-- TODO: testing configuration (?)

-- | A test dummy
main :: IO ()
main =
  (liftM and . mapM ($ stdArgs))
    [ C.testCombinatorics
    , F.testForm nmax
    , B.testBernstein
    , MI.testMI
    , S.testSimplex
    , V.testVector
    , Q.testQuadrature
    , P.testPolynomial
    , DF.testDifferentialForm
    , FE.testFiniteElement
    ]
  >>= flip unless exitFailure
