module Main where

import qualified FEECa.Internal.FormTest as F
import FEECa.Internal.MultiIndexTest
import FEECa.Internal.SimplexTest
import FEECa.Internal.VectorTest

import FEECa.Utility.CombinatoricsTest
-- import FEECa.Utility.GramSchmidtTest
import FEECa.Utility.QuadratureTest
import FEECa.Utility.Test

import FEECa.BernsteinTest
import FEECa.FiniteElementTest
import FEECa.PolynomialTest
import FEECa.PolynomialDifferentialFormTest

import System.Exit (exitFailure)

-- | A test dummy
main :: IO ()
main = do
  putStrLn "TODO: appropriate tests + testing configuration (?)"
  exitFailure

