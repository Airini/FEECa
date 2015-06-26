{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FEEC.Bernstein where

import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Internal.Simplex
import FEEC.Internal.Spaces hiding (pow)
import FEEC.Internal.Vector

import FEEC.Polynomial hiding (Constant, constant, monomial)
import qualified FEEC.Polynomial as P (multiIndices, monomial, constant)

import FEEC.Utility.Utility
import FEEC.Utility.Print

import Math.Combinatorics.Exact.Factorial
import Math.Combinatorics.Exact.Binomial

-- TODO: Enforce consistency of polynomial and simplex.

-- | Bernstein polynomial over a simplex. Represented by a normal polynomial
-- | internally and uses the generalized functions for evaluation and derivation.
data BernsteinPolynomial = Bernstein Simplex (Polynomial Double) |
                           Constant Double
--  deriving Show

-- pretty printing for Bernstein polyonmials
instance Show BernsteinPolynomial where
    show (Bernstein t p) = show $ printPolynomial0 lambda (map expandTerm (terms p))
    show (Constant p) = show $ printPolynomial0 lambda (map expandTerm (terms (P.constant p)))


-- | Bernstein polynomials as a vector space.
instance VectorSpace BernsteinPolynomial where
    type Scalar BernsteinPolynomial = Double
    addV = addB
    sclV = sclB

-- | Bernstein polynomials as a ring.
instance Ring BernsteinPolynomial where
    add = addB
    addId = Constant 0.0
    addInv = sclB (-1)

    mul = mulB
    mulId = Constant 1.0

    fromInt = Constant . fromIntegral

instance Function BernsteinPolynomial Vector where
  type Values   BernsteinPolynomial Vector = Double
  type GeomUnit BernsteinPolynomial Vector = Simplex
  deriv = deriveB
  integrate sx bp@(Constant a) = integralB $ Bernstein sx (constPInN a (geometricalDimension sx))
  integrate _sx bp@(Bernstein _sx' p) = integralB bp -- TODO: check equality for sx and sx'??
  eval = evalB


-- | Create a Bernstein monomial over a given simplex from a given
-- | multi-index.
monomial :: Simplex -> MI.MultiIndex -> BernsteinPolynomial
monomial t mi = Bernstein t (P.monomial mi)

-- | Create a constant bernstein monomial.
constant :: Double -> BernsteinPolynomial
constant = Constant

-- | Derivative of a Bernstein monomial
deriveMonomial :: Simplex -> Int -> MI.MultiIndex -> [Term Double]
deriveMonomial t d mi
    | d < dim mi = [Term (r i * dbs i) (MI.dec d mi) | i <- [0..n]]
    | otherwise = error "deriveMonomial: Direction and multi-index have unequal lengths"
  where mi' = MI.toList mi
        r i = fromInteger (mi' !! i)
        bs = barycentricCoordinates t
        dbs i = eval (unitVector n d) (deriv (unitVector n d) (bs !! i))
        n = geometricalDimension t

-- | Derive Bernstein polynomial.
deriveB :: Vector -> BernsteinPolynomial -> BernsteinPolynomial
deriveB v (Bernstein t p) = Bernstein t (derive (deriveMonomial t) v p)
deriveB v (Constant c)    = Constant 0

-- | Add Bernstein polynomials.
addB :: BernsteinPolynomial -> BernsteinPolynomial -> BernsteinPolynomial
addB (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t2 = error "addB: Cannot add Bernstein polynomials defined over different simplices."
     | otherwise = Bernstein t1 (add p1 p2)
addB (Constant c)    (Bernstein t p) = Bernstein t (add p (P.constant c))
addB (Bernstein t p) (Constant c)    = Bernstein t (add p (P.constant c))
addB (Constant c1)   (Constant c2)   = Constant (c1 + c2)

-- | Scale Bernstein polynomial.
sclB :: Double -> BernsteinPolynomial -> BernsteinPolynomial
sclB c  (Bernstein t p) = Bernstein t (sclV c p)
sclB c1 (Constant c2)   = Constant (c1 * c2)

-- | Multiply two Bernstein polynomials.
mulB :: BernsteinPolynomial -> BernsteinPolynomial -> BernsteinPolynomial
mulB (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t1 = error "mulB: Cannot multiply two Bernstein polynomials defined over different simplices."
     | otherwise = Bernstein t1 (mul p1 p2)
mulB (Constant c)      (Bernstein t1 p1) = Bernstein t1 (sclV c p1)
mulB (Bernstein t1 p1) (Constant c)      = Bernstein t1 (sclV c p1)
mulB (Constant c1)     (Constant c2)     = Constant (c1 * c2)

-- | Evaluat a Bernstein monomial over a given simplex at Vector
-- TODO: change vector to point
evalMonomial :: Simplex -> Vector -> MI.MultiIndex -> Double
evalMonomial t v mi = prefactor n mi * pow (vector lambda) mi
    where lambda = map (eval v) (barycentricCoordinates t)
          n = geometricalDimension t

-- | Evaluation of Bernstein polynomials.
evalB :: Vector -> BernsteinPolynomial -> Double
evalB v (Bernstein t p) = evaluate (evalMonomial t) v p
evalB v (Constant c) = c

-- | List multi-indices of the terms in the polynomial.
multiIndices :: BernsteinPolynomial -> [MI.MultiIndex]
multiIndices (Bernstein t p) = P.multiIndices n p
    where n = geometricalDimension t

-- | Prefactor for Bernstein polynomials.
prefactor :: Int -> MI.MultiIndex -> Double
prefactor n a = fromInteger (factorial n) / fromInteger (MI.factorial a)

-- | Projection fuction for gradients of barycentric coordinates as basis for
-- | the space of alternating forms.
proj :: Simplex -> Int -> Vector -> BernsteinPolynomial
proj t i v = Bernstein t (P.constant $ sum (zipWith (*) grad (toList v)))
    where grad = barycentricGradient t i

-- TODO: Check if really needed?
degRPolynomials :: Simplex -> Int -> Int -> [BernsteinPolynomial]
degRPolynomials t n r = [monomial t mi | mi <- MI.degreeR n r]

-- | Closed-form integration of Bernstein polynomials over the simplex they are
-- | defined over.
integrateBernstein :: BernsteinPolynomial -> Double
integrateBernstein (BernsteinPolynomial t p)  = sum (map f (degrees p))
    where f mi = vol / ((n + MI.deg mi) `choose` n)
          vol = volume t 

-- | Integrate Bernstein polynomial over the simplex it is defined over.
integralB :: BernsteinPolynomial -> Double
integralB (Constant _) = error "integral: Cannot integrate Bernstein polynomial without associated simplex."
integralB (Bernstein t p) = integrate n f p
    where n = geometricalDimension t
          f mi = volume t / fromIntegral ((n + MI.deg mi) `choose` MI.deg mi)

-- | Extend a Bernstein polynomial defined on a subsimplex f to the simplex t.
extend :: Simplex -> BernsteinPolynomial -> BernsteinPolynomial
extend t (Bernstein f p) = Bernstein t (polynomial (extend' (toPairs n' p)))
    where extend' = map (\(c, mi) -> (c, MI.extend n (sigma f) mi))
          n = topologicalDimension t
          n' = topologicalDimension f
extend _ c = c
