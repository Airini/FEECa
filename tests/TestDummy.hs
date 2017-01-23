module Main where

import qualified FEECa.Internal.FormTest        as F
import qualified FEECa.Internal.MultiIndexTest  as MI
import qualified FEECa.Internal.SimplexTest     as S
import qualified FEECa.Internal.VectorTest      as V

import qualified FEECa.Utility.CombinatoricsTest  as C
-- import FEECa.Utility.GramSchmidtTest
import qualified FEECa.Utility.QuadratureTest     as Q
-- import qualified FEECa.Utility.Test               as U

import qualified FEECa.BernsteinTest      as B
import qualified FEECa.FiniteElementTest  as FE
import qualified FEECa.PolynomialTest     as P
import qualified FEECa.PolynomialDifferentialFormTest as DF

import System.Exit (exitSuccess, exitFailure)

nmax :: Int
nmax = 4

-- | A test dummy
main :: IO ()
main = do
  C.testCombinatorics
  F.main nmax
  B.testBernstein
  MI.testMI
  S.testSimplex
  V.main
  Q.testQuadrature
  P.testPolynomial
  DF.testDifferentialForm
  FE.testFiniteElement
  putStrLn "TODO: appropriate tests + testing configuration (?)"
  --exitFailure
  exitSuccess -- TODO: update to catch failures above

