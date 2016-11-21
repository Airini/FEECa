module Main where

import qualified FEECa.Internal.FormTest as F
import FEECa.Internal.MultiIndexTest
import FEECa.Internal.SimplexTest
import FEECa.Internal.VectorTest

import qualified FEECa.Utility.CombinatoricsTest as C
-- import FEECa.Utility.GramSchmidtTest
import FEECa.Utility.QuadratureTest
import FEECa.Utility.Test

import FEECa.BernsteinTest
import FEECa.FiniteElementTest
import FEECa.PolynomialTest
import FEECa.PolynomialDifferentialFormTest

import System.Exit (exitFailure)

nmax :: Int
nmax = 4

-- | A test dummy
main :: IO ()
main = do
  C.testCombinatorics
  F.main nmax
  putStrLn "TODO: appropriate tests + testing configuration (?)"
  exitFailure

