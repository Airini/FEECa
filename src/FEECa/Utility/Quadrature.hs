module FEECa.Utility.Quadrature where

import Math.Combinatorics.Exact.Factorial
import Numeric.LinearAlgebra.HMatrix
-- import Numeric.LinearAlgebra.Data

-- | Type synomnym to represent a quadrature as a list pairs
-- | of nodes and corresponding weights, the first element being the node
-- | and the second the weight.
type Quadrature = [(Double, Double)]

-- | Type synomym for the coefficients a_i, b_i, c_i of the recurrence relation of
-- | a given est of orthogonal polynomials.
type Coeff a = (Int -> a)

-- | Compute the Gauss-Jacobi quadrature of degree n and parameters alpha = a
-- | and beta = b.
gaussJacobiQuadrature :: Int -> Int -> Int -> Quadrature
gaussJacobiQuadrature = golubWelsch

-- | Compute the Gauss-Jacobi quadrature of degree n and parameters alpha = a
-- | and beta = b shifted and scaled to the interval [0,1].
gaussJacobiQuadrature' :: Int -> Int -> Int -> Quadrature
gaussJacobiQuadrature' alpha beta n = transform (gaussJacobiQuadrature alpha beta n)
    where transform = map (\(x,y) -> ( scale1 x, scale2 y))
          scale1 x = 0.5 + x/2
          scale2 y = y

-- | Compute the Gauss-Legendre quadrature of degree n.
gaussLegendreQuadrature :: Int -> Quadrature
gaussLegendreQuadrature = golubWelsch 0 0

-- | Golub-Welsch algorithm as described in "Calculation of Gauss Quadrature
-- | Rules" (Golub, Welsch).
golubWelsch :: Int -> Int -> Int -> Quadrature
golubWelsch alpha beta n = extract $ eigSH' (buildMatrix n a b c)
    where a = a_jac alpha beta
          b = b_jac alpha beta
          c = c_jac alpha beta
          extract (v, m) = zip (toList v)
                               (square (toList (head (toRows m))))
          square  = map (** 2)
          -- TODO: why is scale defined but not used?
          -- normalize l = map ((2 / sum l) *) l
          -- alpha' = fromIntegral alpha
          -- scale = map ((2.0**(1.0+ alpha') / (1 + alpha')) *)

gamma :: Int -> Int -> Double
gamma a b = fromInteger (2^(a + b + 1) * factorial a * factorial b) /
            fromInteger (factorial (a + b + 1))

-- Build the tridiagonal matrix used in the Golub-Welsch algorithm.
buildMatrix :: Int -> Coeff Double -> Coeff Double -> Coeff Double -> Matrix Double
buildMatrix n a b c = (n >< n) $ concat [ p1 i ++ [p2 i] ++ p3 i | i <- [1..n] ]
    where p1 i = replicate (i - 2) 0 ++ f1 i
          p2 i = - (b i / a i)
          p3 i = f2 i ++ replicate (n - i - 1) 0
          beta i = sqrt (c (i + 1) / (a i* a (i+1)))
          f1 i = [ beta (i-1) | i > 1 ]
          f2 i = [ beta  i    | i < n ]


-- | Evaluate an orthogonal Polynomial using the coefficient functions describing
-- | its recurrence relation.
orthogonalPolynomial :: Int -> Int -> Int -> Double -> Double
orthogonalPolynomial n alpha beta x
    | n == 0     = 1.0
    | n > 0      = orthogonalPolynomial' n 1 alpha beta x 1.0 0.0
    | otherwise  = error $ "orthogonalPolynomial: n = " ++ show n ++ " < 0"

-- Iterative implementation of the recursion relations for orthogonal polynomials.
-- See article by Golub and Welsch.
orthogonalPolynomial' ::
  Int -> Int -> Int -> Int ->
  Double -> Double -> Double -> Double
orthogonalPolynomial' n i alpha beta x pi1 pi2
    | n == i     = p_i
    | otherwise  = orthogonalPolynomial' n (i+1) alpha beta x p_i pi1
    where  p_i  = (a i * x + b i) * pi1 - c i * pi2
           a    = a_jac alpha beta
           b    = b_jac alpha beta
           c    = c_jac alpha beta

-- a_i coefficient for Jacobi polynomials as defined in the paper by
-- Golub and Welsch.
a_jac :: Int -> Int -> Coeff Double
a_jac alpha beta i
    | (2*i + alpha + beta) <= 2 = 1
    | otherwise = i2ab * (i2ab - 1) * (i2ab -2) / den_jac alpha beta i
  where i2ab = fromIntegral (2 * i + alpha + beta)

-- b_i coefficient for Jacobi polynomials as defined in the paper by
-- Golub and Welsch.
b_jac :: Int -> Int -> Coeff Double
b_jac alpha beta i
    | (2*i + alpha + beta) <= 2 = 0
    | otherwise = (2*i' + a' + b' - 1) * (a'*a' - b'*b') / den_jac alpha beta i
  where a' = fromIntegral alpha
        b' = fromIntegral beta
        i' = fromIntegral i

-- c_i coefficient for Jacobi polynomials as defined in the paper by
-- Golub and Welsch.
c_jac :: Int -> Int -> Coeff Double
c_jac alpha beta i
    | (2*i + alpha + beta) <= 2 = 0
    | otherwise = 2 * (i' + a' - 1) * (i' + b' - 1) * (2 * i' + a' + b') /
                  den_jac alpha beta i
  where a' = fromIntegral alpha
        b' = fromIntegral beta
        i' = fromIntegral i

-- Common denominator for the coefficient functions of the recurrence relation
-- for the Jacobi integrals.
den_jac :: Int -> Int -> Int -> Double
den_jac alpha beta i =
    fromIntegral (2 * i * (i + alpha + beta) * (2 * i + alpha + beta - 2))

-- | Evaluate Jacobi Polynomial.
jacobiP :: Int -> Int -> Int -> Double -> Double
jacobiP alpha beta n = orthogonalPolynomial n alpha beta

-- | Evaluate Legendre polynomial of degree n at x.
legendreP :: Int -> Double -> Double
legendreP n = orthogonalPolynomial n 0 0
