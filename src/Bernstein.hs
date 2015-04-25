{-# LANGUAGE TypeFamilies #-}

module Bernstein where

import Simplex
import Spaces
import Vector
import Polynomials hiding(Constant)
import Utility
import Print
import MultiIndex
import Math.Combinatorics.Exact.Factorial
import Math.Combinatorics.Exact.Binomial


-- | Bernstein polynomial over a simplex. Represented by a normal polynomial
-- | internally and uses the generalized functions for evaluation and derivation.
data BernsteinPolynomial = Bernstein Simplex (Polynomial Double) |
                           Constant Double

-- pretty printing for Bernstein polyonmials
instance Show BernsteinPolynomial where
    show (Bernstein t p) = show $ printPolynomial lambda (map expandTerm (terms p))

-- | Bernstein polynomials as a vector space.
instance VectorSpace BernsteinPolynomial where
    type Fieldf BernsteinPolynomial = Double
    vspaceDim (Bernstein t p) = undefined
    addV = addB
    sclV = sclB

-- | Bernstein polynomials as a field.
instance Field BernsteinPolynomial where
    add = addB
    addId = Constant 0.0
    addInv = sclB (-1)

    mul = mulB
    mulId = Constant 1.0
    mulInv = undefined

    fromInt = Constant . fromIntegral

-- | Create a bernstein monomial.
bernsteinMonomial :: Simplex -> MultiIndex -> BernsteinPolynomial
bernsteinMonomial t mi = Bernstein t (monomial mi)

-- | Create a constant bernstein monomial.
constantB :: Double -> BernsteinPolynomial
constantB c = Constant c

-- | Derivative of a Bernstein monomial
deriveMonomial :: Simplex -> Int -> MultiIndex -> [Term Double]
deriveMonomial t d mi
  | d < dim mi = [Term ((r i) * (dbs i)) (decMI d mi) | i <- [0..n]]
  | otherwise = error "deriveMonomial: Direction and multi-index have unequal lengths"
  where mi' = toListMI mi
        r i = fromInteger (mi' !! i)
        bs = barycentricCoordinates t
        dbs i = eval (unitV n d) (deriv (unitV n d) (bs !! i))
        n = geometricalDimension t

-- | Derive Bernstein polynomial.
deriveB :: Vector -> BernsteinPolynomial -> BernsteinPolynomial
deriveB v (Bernstein t p) = Bernstein t (derive (deriveMonomial t) v p)
deriveB v (Constant c) = (Constant 0)

-- | Add Bernstein polynomials.
addB :: BernsteinPolynomial -> BernsteinPolynomial -> BernsteinPolynomial
addB (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t2 = error "addB: Cannot add Bernstein polynomials defined over different simplices."
     | otherwise = Bernstein t1 (add p1 p2)
addB (Constant c) (Bernstein t  p) = Bernstein t (add p (constant c))
addB (Bernstein t  p) (Constant c) = Bernstein t (add p (constant c))
addB (Constant c1) (Constant c2) = Constant (c1 + c2)

-- | Scale Bernstein polynomial.
sclB :: Double -> BernsteinPolynomial -> BernsteinPolynomial
sclB c (Bernstein t p) = (Bernstein t (sclV c p))
sclB c1 (Constant c2) = Constant (c1 * c2)

-- | Multiply two Bernstein polynomials.
mulB :: BernsteinPolynomial -> BernsteinPolynomial -> BernsteinPolynomial
mulB (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t1 = error "mulB: Cannot multiply two Bernstein polynomials defined over different simplices."
     | otherwise = Bernstein t1 (mul p1 p2)
mulB (Constant c) (Bernstein t1 p1) = Bernstein t1 (sclV c p1)
mulB (Bernstein t1 p1) (Constant c) = Bernstein t1 (sclV c p1)
mulB (Constant c1) (Constant c2) = Constant (c1 * c2)

-- | Evaluat a Bernstein monomial over a given simplex at Vector
-- TODO: change vector to point
evalMonomial :: Simplex -> Vector -> MultiIndex -> Double
evalMonomial t v mi = (prefactor n mi) * (powV (vector lambda) mi)
    where lambda = map (eval v) (barycentricCoordinates t)
          n = geometricalDimension t

-- | Evaluation of Bernstein polynomials.
evalB :: Vector -> BernsteinPolynomial -> Double
evalB v (Bernstein t p) = evaluate (evalMonomial t) v p
evalB v (Constant c) = c

-- | Prefactor for Bernstein polynomials.
prefactor :: Int -> MultiIndex -> Double
prefactor n a = (fromInteger (factorial n)) / fromInteger (factorialMI a)

-- | Projection fuction for gradients of barycentric coordinates as basis for
-- | the space of alternating forms.
proj :: Simplex -> Int -> Vector -> BernsteinPolynomial
proj t i v = Bernstein t (constant $ sum (zipWith (*) grad (toList v)))
    where grad = barycentricGradient t i

degRPolynomials :: Simplex -> Int -> Int -> [BernsteinPolynomial]
degRPolynomials t n r = [Bernstein t (monomial mi) | mi <- degRMI n r]
