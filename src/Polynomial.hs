{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Polynomial (
  -- * Polynomial types
  Polynomial(..), Term(Term)


  -- * Predefined constructors

  -- ** Primitive constructors
  , polynomial, constant, constPInN, deg0P, deg1P
  
  -- ** Derived constructors
  , barycentricCoordinate, barycentricCoordinates


  -- * Polynomial operations
  
  -- ** Mathematical operations
  , derive, evaluate, integralP
  
  -- ** Manipulation operations
  , expandTerm, monomial, multiIndices, toPairs

  -- ** Simplex dependent operations
  , barycentricGradient, barycentricGradients
  ) where

import Spaces hiding (toList)
import Simplex
import Vector
import Point
import Utility(pairM)
import Print (printPolynomial)
import Text.PrettyPrint
import Data.Maybe (fromJust)
import Data.List
import qualified MultiIndex as MI (MultiIndex, zero, one, dec, toList, add, deg, valid)
import qualified Numeric.LinearAlgebra.HMatrix as M
import qualified Numeric.LinearAlgebra.Data as M

-- | Represents terms of a n-dimensional polynomial. A Term is either a constant
-- | with a given value or consists of a multi-index and a double representing a
-- | monomial scaled by a scalar.
data Term a = Constant a | Term a MI.MultiIndex
            deriving Eq

-- | Type synonym for a function to generalize the derivative of a monomial
-- | in a given space direction. Required to generalize the polinomial code to
-- | different bases.
type Dx = Int -> MI.MultiIndex -> [Term Double]

-- | Return the coefficient of a term of a polynomial.
coeff :: Term a -> a
coeff (Constant c) = c
coeff (Term c _)   = c

-- | Scale term by a scalar.
sclTerm :: Field a => a -> Term a -> Term a
sclTerm c1 (Term c2 mi)  = Term (mul c1 c2) mi
sclTerm c1 (Constant c2) = Constant (mul c1 c2)

-- | Multiply two terms.
mulTerm :: Field a => Term a -> Term a -> Term a
mulTerm (Term c1 mi1) (Term c2 mi2) = Term (mul c1 c2) (MI.add mi1 mi2)
mulTerm (Term c1 mi) (Constant c2) = Term (mul c1 c2) mi
mulTerm (Constant c2) (Term c1 mi) = Term (mul c1 c2) mi
mulTerm (Constant c1) (Constant c2) = Constant (mul c1 c2)

-- | Evaluate monomial over standard monomial basis.
evalMonomial :: Vector -> MI.MultiIndex -> Double
evalMonomial = powV

-- | General evaluation of a term. Given a function for the evaluation a
-- | monomial, the function returns the corresponding value of the polynomial
-- | scaled by the terms coefficient or simply the value of the term if the term
-- | is constant.
evalTerm :: (Vector -> MI.MultiIndex -> Double) -> Vector -> Term Double -> Double
evalTerm f v (Term c mi)  = c * f v mi
evalTerm f v (Constant c) = c

-- | General derivative of a term. Given a function for the derivative of a monomial
-- | in a given space direction, the function computes the derivative of the given
-- | term using the product rule.
deriveTerm :: Dx -> Vector -> Term Double -> [Term Double]
deriveTerm dx v (Constant _) = [Constant 0]
deriveTerm dx v (Term c mi)  = concat [map (sclTerm (v' !! i)) (dx i mi) |
                                       i <- [0..n-1],
                                       MI.deg mi > 0]
    where
      v' = toList v
      n = dim v

-- | Derivative of a monomial over the standard monomial basis in given space
-- | direction.
deriveMonomial :: Dx
deriveMonomial i mi
    | i < dim mi = [Term c (MI.dec i mi)]
    | otherwise = error "deriveMonomial: Direction and multi-index have unequal lengths"
  where c  = fromInt (MI.toList mi !! i)

-- | General polynomial type. Represents a multi-dimensional polynomial of given
-- | degree by a list of terms. A term may either be a monomial scaled by a scalar,
-- | represented by Double and a MI.MultiIndex, or a constant, represented simply by a
-- | Double. The length of the multi-indices must match the dimensionality of the
-- | underlying vector space.
data Polynomial a =
    Polynomial { degree :: Int,
                 terms  :: [Term a] }
    deriving Eq

-- | Polynomials as vector spaces.
instance VectorSpace (Polynomial Double) where
  type Fieldf (Polynomial Double) = Double
  addV = addP
  sclV = sclP

-- | Polynomials as a field.
instance Field f => Field (Polynomial f) where
  add    = addP
  addId  = constant addId
  addInv = sclP (addInv addId)

  mul       = mulP
  mulId     = constant mulId
  mulInv    = undefined

  fromInt x = Polynomial 0 [Constant (fromInt x)]

-- | Polynomials as functions.
instance Function (Polynomial Double) Vector where
  type Values (Polynomial Double) Vector = Double
  type GeomUnit (Polynomial Double) Vector = Simplex
  integrate = undefined
  deriv = deriveP
  eval = evalP

-- | Create a constant polynomial with the given value.
constant :: a -> Polynomial a
constant c = Polynomial 0 [Constant c]

-- | Create a polynomial consisting of a single monomial from a give
-- | multi-index.
monomial :: Field a => MI.MultiIndex -> Polynomial a
monomial mi = Polynomial (MI.deg mi) [Term mulId mi]

-- | Create a term of a polynomial consisting of a scaled monomial.
term :: Field a => (a, MI.MultiIndex) -> Term a
term (c, mi) = Term c mi

-- | Create a polynomial from a list of coefficient-multi-index pairs.
polynomial :: Field a => [(a, MI.MultiIndex)] -> Polynomial a
polynomial l = if (checkPolynomial l)
               then Polynomial r (map term l)
               else error "Given coefficients and multi-indices do not define a valid polynomial."
    where r = maximum (map (MI.deg . snd) l)

-- | Check whether a list of coefficient-multi-index pairs represents a
-- | polynomial.
checkPolynomial :: [(a, MI.MultiIndex)] -> Bool
checkPolynomial ls = (all (MI.valid . snd) ls) && (sameLength (map snd ls))

-- | Check if all multi-indices in the list have the same dimension.
sameLength :: [MI.MultiIndex] -> Bool
sameLength (l:ls) = sameLength' (dim l) ls
sameLength [] = True

sameLength' :: Int -> [MI.MultiIndex] -> Bool
sameLength' i (l:ls) = (i == (dim l)) && (sameLength' (dim l) ls)
sameLength' _ [] = True

-- | Returns a list of the multi-indices in the polynomial.
multiIndices :: Int -> Polynomial a -> [MI.MultiIndex]
multiIndices n (Polynomial _ ls) = multiIndices' n ls

multiIndices' :: Int -> [Term a] -> [MI.MultiIndex]
multiIndices' n (Term _ mi  : ls) = mi : multiIndices' n ls
multiIndices' n (Constant _ : ls) = MI.zero n : multiIndices' n ls
multiIndices' _ [] = []

-- | Expands Constant types in the list of terms and returns a list of all
-- | coefficient multi-index pairs in the polynomial.
toPairs :: Int -> Polynomial a -> [ (a, MI.MultiIndex) ]
toPairs n p = toPairs' n (terms p)

toPairs' :: Int -> [Term a] -> [ (a, MI.MultiIndex) ]
toPairs' n (Term c mi  : ls) = (c, mi) : toPairs' n ls
toPairs' n (Constant c : ls) = (c, MI.zero n) : toPairs' n ls
toPairs' _ [] = []

-- | Pretty printing of polynomials.
instance Show (Polynomial Double) where
    show p = show $ printPolynomial "x" (map expandTerm (terms p))

-- | Expand term to (Double, MI.MultiIndex)-form suitable for printing.
expandTerm :: Term Double -> (Double, MI.MultiIndex)
expandTerm (Constant c) = (c, MI.zero 0)
expandTerm (Term c mi) = (c ,mi)

-- | Add two polynomials.
addP :: (Field a) => Polynomial a -> Polynomial a -> Polynomial a
addP (Polynomial r1 ts1) (Polynomial r2 ts2) =
    Polynomial (max r1 r2) (ts1 ++ ts2)

-- | Scaling of a polynomial.
sclP :: (Field a) => a -> Polynomial a -> Polynomial a
sclP c (Polynomial r ts) = Polynomial r (map (sclTerm c) ts)

-- | Polynomial multiplication.
mulP :: (Field a) => Polynomial a -> Polynomial a -> Polynomial a
mulP (Polynomial r1 ts1) (Polynomial r2 ts2) =
    Polynomial (r1 + r2) [mulTerm t1 t2 | t1 <- ts1, t2 <- ts2]

-- | General evaluation function of a polynomial using the given function for
-- | the evaluation of monomials.
evaluate :: (Vector -> MI.MultiIndex -> Double) -> Vector -> Polynomial Double -> Double
evaluate f v (Polynomial r ts) = foldl add addId (map (evalTerm f v) ts)

-- | Evaluate polynomial at given point in space.
evalP :: Vector -> Polynomial Double -> Double
evalP = evaluate evalMonomial

-- | General derivative for a polynomial with arbitrary basis.
derive :: Dx -> Vector -> Polynomial Double -> Polynomial Double
derive dx v (Polynomial r ts) = Polynomial (r - 1) (concatMap (deriveTerm dx v) ts)

-- | Directional derivative of a polynomial in a given space direction.
deriveP :: Vector -> Polynomial Double -> Polynomial Double
deriveP = derive deriveMonomial

-- | General integral of a polynomial.
integralP :: Field a => Int -> (MI.MultiIndex -> a) -> Polynomial a -> a
integralP n f p@(Polynomial r ts) = foldl add addId [ mul c ( f mi ) | (c, mi) <- toPairs n p ]

-- | Create constant polynomial
deg0P :: a -> Polynomial a
deg0P = constant

-- | Create a constant polynomial in a specific n-dimension space
constPInN :: a -> Int -> Polynomial a
constPInN c n = Polynomial 0 [Term c $ MI.zero n]

-- | Create 1st degree homogeneous polynomial in n variables from
-- | length n list of coefficients. The coefficient with index i in the list
-- | equals the coefficient of the ith variable of the returned polynomial.
deg1P :: [a] -> Polynomial a
deg1P ns = Polynomial 1 $ zipWith Term ns [MI.one dim i | i <- [0..dim-1]]
  where dim  = length ns

-- | 1st degree polynomial taking value 1 on vertex n_i of the simplex and
-- | 0 on all others. Requires the topological dimension of the simplex to be
-- | as large as the geometrical dimension, i.e. the simplex must contain n+1
-- | vertices if the underlying space has dimensionality n.
-- TODO: check take
barycentricCoordinates :: Simplex -> [Polynomial Double]
barycentricCoordinates s = map vectorToPolynomial (take (nt+1) (M.toColumns mat))
    where mat = M.inv (simplexToMatrix (extendSimplex s))
          n   = geometricalDimension s
          nt  = topologicalDimension s

-- | Simple wrapper for barycentricCoordinates that picks out the ith polynomial
-- | in the list
barycentricCoordinate :: Simplex -> Int -> Polynomial Double
barycentricCoordinate s i = barycentricCoordinates s !! i

-- | Compute gradients of barycentric coordinates as a list of lists of Double
-- | for the given simplex t
barycentricGradients :: Simplex -> [[Double]]
barycentricGradients t = map vectorToGradient (take (nt+1) (M.toColumns mat))
    where mat = M.inv (simplexToMatrix (extendSimplex t))
          n = geometricalDimension t
          nt = topologicalDimension t

-- | Compute gradient of the barycentric coordinate corresponding to edge i
barycentricGradient :: Simplex -> Int -> [Double]
barycentricGradient t i = barycentricGradients t !! i

-- Transforms a given simplex into the matrix representing the linear
-- equation system for the barycentric coordinates.
simplexToMatrix :: Simplex -> M.Matrix Double
simplexToMatrix s@(Simplex _ l) = M.matrix (n+1) (concatMap append1 l)
    where n = geometricalDimension s
          append1 p = 1 : toList (fromPoint p)

-- Transforms a solution vector of the linear equation system for the
-- barycentric coordinates into the corresponding polynomial.
vectorToPolynomial :: M.Vector Double -> Polynomial Double
vectorToPolynomial v = add (sclV (head l) mulId) (deg1P (tail l))
    where l = M.toList v
          n = length l - 1

-- Transforms a solution vector of the linear equation system into the
-- gradients of the barycentric coordinates.
vectorToGradient :: M.Vector Double -> [Double]
vectorToGradient = tail . M.toList

