module Bernstein where

import Simplex
import Spaces
import Vector
import Polynomials
import Utility
import Print
import MultiIndex(MultiIndex,addMI,decMI,chooseMI,degMI)
import Math.Combinatorics.Exact.Factorial
import Math.Combinatorics.Exact.Binomial

data BernsteinPolynomial = Bernstein Simplex [(Double,MultiIndex)]

instance Show BernsteinPolynomial where
    show (Bernstein _ p) = show $ printPolynomial "\x03BB" p'
        where p' = map scl p
              scl (c,a) = (c*(prefactor (degMI a) a),a)

addB :: BernsteinPolynomial -> BernsteinPolynomial -> BernsteinPolynomial
addB (Bernstein t1 l1) (Bernstein t2 l2)
    | t1 == t2 = Bernstein t1 (l1 ++ l2)
    | otherwise = error "addB: Bernstein polynomials defined over different vertices"

mulB :: BernsteinPolynomial -> BernsteinPolynomial -> BernsteinPolynomial
mulB (Bernstein t1 l1) (Bernstein t2 l2)
    | t1 == t2 = Bernstein t1 [mulMonomial m1 m2 | m1 <- l1, m2 <- l2]
    | otherwise = error "addB: Bernstein polynomials defined over different vertices"

mulMonomial :: (Double, MultiIndex) -> (Double, MultiIndex) -> (Double, MultiIndex)
mulMonomial (c1, a1) (c2, a2) = (c*c1*c2, a)
    where a = addMI a1 a2
          c = fromIntegral $ (chooseMI a a1) / (choose (r+s) r)
          r = degMI a1
          s = degMI a2

deriveB :: Vector -> BernsteinPolynomial -> BernsteinPolynomial
deriveB v (Bernstein t l) = Bernstein t (concat (zipWith (deriveMonomial dbs) [0..n] l))
    where dbs = map ((eval zeroV).(deriv v)) bs
          bs = barycentricCoords t
          zeroV = vector $ replicate (dim v) (0.0::Double)
          n = dim v

deriveMonomial :: [Double] -> Int -> (Double, MultiIndex) -> [(Double,MultiIndex)]
deriveMonomial dbs i (c,a) = [(c*r*db, decMI i a) |  i <- [0..degMI a]]
    where r = fromIntegral (sum a)
          db = (dbs !! i)

prefactor :: Int -> MultiIndex -> Double
prefactor r a = (fromInteger (factorial r)) / (fromInt (foldl (*) 1 a))

-- gradB :: BernsteilPolynomial -> BernsteinPolynomial
-- gradB (Bernstein t l) =
