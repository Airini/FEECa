module Bernstein where

import Simplex
import Spaces
import Vector
import Polynomials
import Utility
import Print
import MultiIndex(MultiIndex,addMI,oneMI,decMI,chooseMI,degMI,toListMI,factorialMI)
import Math.Combinatorics.Exact.Factorial
import Math.Combinatorics.Exact.Binomial

-- | Bernstein polynomial over a simplex
data BernsteinPolynomial = Bernstein Simplex [(Double,MultiIndex)]

instance Show BernsteinPolynomial where
    show (Bernstein _ p) = show $ printPolynomial "\x03BB" p'
        where p' = map scl p
              scl (c,a) = (c*(prefactor (degMI a) a),a)

-- | Addition
addB :: BernsteinPolynomial -> BernsteinPolynomial -> BernsteinPolynomial
addB (Bernstein t1 l1) (Bernstein t2 l2)
    | t1 == t2 = Bernstein t1 (l1 ++ l2)
    | otherwise = error "addB: Bernstein polynomials defined over different vertices"

-- | Multiplication
mulB :: BernsteinPolynomial -> BernsteinPolynomial -> BernsteinPolynomial
mulB (Bernstein t1 l1) (Bernstein t2 l2)
    | t1 == t2 = Bernstein t1 [mulMonomial m1 m2 | m1 <- l1, m2 <- l2]
    | otherwise = error "addB: Bernstein polynomials defined over different vertices"

mulMonomial :: (Double, MultiIndex) -> (Double, MultiIndex) -> (Double, MultiIndex)
mulMonomial (c1, a1) (c2, a2) = (c*c1*c2, a)
    where a = addMI a1 a2
          c = (fromIntegral (chooseMI a a1)) / (fromIntegral (choose (r+s) r))
          r = fromInteger (degMI a1)
          s = fromInteger (degMI a2)

-- | Derivation
deriveB :: Vector -> BernsteinPolynomial -> BernsteinPolynomial
deriveB v (Bernstein t l) = Bernstein t (concat (zipWith (deriveMonomial dbs) [0..n] l))
    where dbs = map ((eval zeroV).(deriv v)) bs
          bs = barycentricCoordinates t
          zeroV = vector $ replicate (dim v) (0.0::Double)
          n = dim v

deriveMonomial :: [Double] -> Int -> (Double, MultiIndex) -> [(Double,MultiIndex)]
deriveMonomial dbs i (c,a) = [(c*r*db, decMI i a) |  i <- [0..(degMI a)]]
    where r = fromIntegral (sum (toListMI a))
          db = (dbs !! i)

-- | Evaluate given Bernstein polynomial at Vector
-- TODO: change vector to point
evalB :: BernsteinPolynomial -> Vector -> Double
evalB (Bernstein t ls) v = foldl (\ x y -> x + (evalMonomial y)) 0 ls
    where evalMonomial (c,mi) = c 1* (powV (vector lambda) mi)
          lambda = map (eval v) (barycentricCoordinates t)

prefactor :: Int -> MultiIndex -> Double
prefactor n a = (fromInteger (factorial n)) / fromInteger (factorialMI a)

-- gradB :: BernsteilPolynomial -> BernsteinPolynomial
-- gradB (Bernstein t l) =

tr2 = referenceSimplex 2
b = Bernstein tr2 [(1.0,oneMI 3 0),(1.0,oneMI 3 1)]
