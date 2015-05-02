module Quadrature where

import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Data

-- | Type synomnym to represent a quadrature as a list pairs
-- | of nodes and corresponding weights, the first element being the node
-- | and the second the weight.
type Quadrature = [(Double, Double)]

-- | Type synomym for the coefficients a_i, b_i, c_i of the recurrence relation of
-- | a given est of orthogonal polynomials.
type Coeff a = (Int -> a)

-- | Compute the Gauss-Jacobi quadrature up to degree n and parameters alpha = a
-- | and beta = b.
gaussJacobiQuadrature :: Int -> Int -> Int -> Quadrature
gaussJacobiQuadrature n a b = golubWelsch n (a_jac a b) (b_jac a b) (c_jac a b)

-- | Golub-Welsch algorithm as described in "Calculation of Gauss Quadrature
-- | Rules" (Golub, Welsch).
golubWelsch :: Int -> Coeff Double -> Coeff Double -> Coeff Double -> Quadrature
golubWelsch n a b c = extract $ eig (buildMatrix n a b c)
    where extract (v, m) = zip (map realPart (toList v))
                               (map realPart (toList (head (toRows m))))

-- Build the tridiagonal matrix used in the Golub-Welsch algorithm.
buildMatrix :: Int -> Coeff Double -> Coeff Double -> Coeff Double -> Matrix Double
buildMatrix n a b c = (n >< n) $ concat [ (p1 i) ++ [p2 i] ++ (p3 i) | i <- [1..n] ]
    where p1 1 = [] :: [Double]
          p1 i = (replicate (i - 2) 0) ++ (f1 i)
          p2 i = - (b i) / (a i)
          p3 i = (f2 i) ++ (replicate (n - i - 1) 0)
          f1 i = if (i > 1) then [ (c i) / (a i) ] else []
          f2 i = if (i < n) then [ 1 / (a i) ] else []

-- a_i coefficient for Jacobi polynomials as defined in the paper by
-- Golub and Welsch.
a_jac :: Int -> Int -> Coeff Double
a_jac a b i = i2ab * (i2ab - 1) * (i2ab -2) / (den_jac a b i)
    where i2ab = fromIntegral (2 * i + a + b)

-- b_i coefficient for Jacobi polynomials as defined in the paper by
-- Golub and Welsch.
b_jac :: Int -> Int -> Coeff Double
b_jac a b i =  (2*i' + a' + b' - 1) * (a'*a' - b'*b') / (den_jac a b i)
    where a' = fromIntegral a
          b' = fromIntegral b
          i' = fromIntegral i

-- c_i coefficient for Jacobi polynomials as defined in the paper by
-- Golub and Welsch.
c_jac :: Int -> Int -> Coeff Double
c_jac a b i = 2 * (i' + a' - 1) * (i' + b' - 1) * (2 * i' + a' + b') / (den_jac a b i)
    where a' = fromIntegral a
          b' = fromIntegral b
          i' = fromIntegral i

-- Common denominator for the coefficient functions of the recurrence relation
-- for the Jacobi integrals.
den_jac :: Int -> Int -> Int -> Double
den_jac a b i = fromIntegral (2 * i * (i + a + b) * (2 * i + a + b - 2))

-- | Evaluate Jacobi Polynomial.
jacobiP :: Int -> Double -> Double
jacobiP n = orthogonalPolynomial n (a_jac 1 0) (b_jac 1 0) (c_jac 1 0)

-- | Evaluate an orthogonal Polynomial using the coefficient functions describing
-- | its recurrence relation.
orthogonalPolynomial :: Int -> Coeff Double -> Coeff Double -> Coeff Double -> Double -> Double
orthogonalPolynomial n a b c x
    | n == 0 = 1.0
    | n > 0 = orthogonalPolynomial' n 1 a b c x 1.0 0.0

-- Iterative implementation of the recursion relations for orthogonal polynomials.
-- See article by Golub and Welsch.
orthogonalPolynomial' :: Int ->
                         Int ->
                         Coeff Double ->
                         Coeff Double ->
                         Coeff Double ->
                         Double ->
                         Double ->
                         Double ->
                         Double
orthogonalPolynomial' n i a b c x pi1 pi2
    | n == i = pi
    | otherwise = orthogonalPolynomial' n (i+1) a b c x pi pi1
    where pi = ((a i) * x + (b i)) * pi1 - (c i) * pi2
