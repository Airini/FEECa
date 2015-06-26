\section{Polynomial}

The \module{Polynomial} module provides data types and functions for polynomials
 in $\R{n}$. Given a vector $\vec{x} \in \R{n}$, the power $\vec{x}$ with
 respect to a multi-index $\vec{\alpha} = (\alpha_0,\ldots,\alpha_{n-1})$
 is given by

\begin{align}
  \vec{x}^{\vec{\alpha}} &= \prod_{i=0}^{n-1} x_i^{\alpha_i}
\end{align}

A polynomial in $\R{n}$ is a function that maps a vector $\vec{x}$ to a
 linear combination of powers of $\vec{x}$:

\begin{align}
  p(\vec{x}) &= \sum_i c_i \vec{x}^{\vec{\alpha}}
\end{align}

Apart from implementing polynomials over vectors in $\R{n}$, the
 \module{Polynomial} module also provides abstract functions for polynomials
 that use different bases, such as the Bernstein polynomials implemented in the
 \module{Bernstein} module.

%------------------------------------------------------------------------------%

\ignore{
\begin{code}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FEEC.Polynomial (

  -- * Polynomial types
  Polynomial(..), Term(Term)

  -- ** Constructors
  , polynomial, constant, linearPolynomial

  -- ** Manipulation operations
  , expandTerm, monomial, multiIndices, degrees

  -- * Mathematical operations
  , derivePolynomial, evaluatePolynomial

  -- * Barycentric Coordinates
  , barycentricCoordinate, barycentricCoordinates
  , barycentricGradient, barycentricGradients


  ) where

import Data.Maybe (fromJust)
import Data.List
import qualified FEEC.Internal.MultiIndex as MI (MultiIndex, zero, one, dec, toList, add, degree, valid)
import FEEC.Internal.Simplex
import FEEC.Internal.Spaces(VectorSpace(..), Ring(..), Dimensioned(..))
import qualified FEEC.Internal.Spaces as S(Function(..))
import FEEC.Internal.Point
import FEEC.Internal.Vector(Vector, toList)
import qualified FEEC.Internal.Vector as V(pow)
import FEEC.Utility.Print(Pretty(..), printPolynomial)
import FEEC.Utility.Utility(pairM)
import Text.PrettyPrint
import qualified Numeric.LinearAlgebra.HMatrix as M
import qualified Numeric.LinearAlgebra.Data as M

\end{code}
}

%------------------------------------------------------------------------------%

\subsection{ The \code{Polynomial} Type}

Two types are used to represent polynomials. The \code{Term} type represents
 a single term in the sum that constitutes the polynomial. The \code{Term} type
 provides two constructors. The \code{Constant} constructor for constant terms
 and the \code{Monomial} constructor for a monomial (represented by a
 \code{MultiIndex}) scaled by a scalar. Note that the constant term carries
 no information about the underlying space while the monomial contains this
 information implicitly in the length of the multi-index. The \code{Term} type
 is parametrized in order to allow for the definition of polynomials over
 arbitrary Rings.

%------------------------------------------------------------------------------%

\begin{code}

-- | Represents terms of a n-dimensional polynomial. A Term is either a constant
-- | with a given value or consists of a multi-index and a double representing a
-- | monomial scaled by a scalar.
data Term a = Constant a
            | Term a MI.MultiIndex
            deriving (Eq, Show)

\end{code}

%------------------------------------------------------------------------------%

Based on the \code{Term} type, a polynomial can now be represented as a list of
 terms, keeping the sum implicit. An additional field \code{degree} that holds
 the degree of polynomial is added for convenience.

Polynomials over $\R{n}$ form a vector space over $\mathrm R$ and a ring. To
 make their algebraic structure available, we make them an instance of
 the \code{Vectorspace} and \code{Ring} classes. The arithmetic functions on
 polynomials are defined below.

%------------------------------------------------------------------------------%

\begin{code}


-- | General polynomial type. Represents a multi-dimensional polynomial of given
-- | degree by a list of terms. A term may either be a monomial scaled by a scalar,
-- | represented by Double and a MI.MultiIndex, or a constant, represented simply by a
-- | Double. The length of the multi-indices must match the dimensionality of the
-- | underlying vector space.
data Polynomial a =
    Polynomial { degree :: Int,
                 terms  :: [Term a] }
    deriving (Eq, Show)

-- | Polynomials as vector spaces.
instance Ring a => VectorSpace (Polynomial a) where
  type Scalar (Polynomial a) = a
  addV = addP
  sclV = sclP

-- | Polynomials as a ring.
instance Ring a => Ring (Polynomial a) where
  add    = addP
  addId  = constant addId
  addInv = sclP (addInv addId)

  mul       = mulP
  mulId     = constant mulId

  fromInt x = Polynomial 0 [Constant (fromInt x)]

\end{code}

%------------------------------------------------------------------------------%

As for all other types, pretty printing of polynomials is provided by
 instantiating the \code{Pretty} class. To hide the internal implementation of
the \code{Polynomial} type, the terms have to be expanded to pairs of
 coefficients and multi-indices before using the rendering functions in the
\module{Print} module.

%------------------------------------------------------------------------------%

\begin{code}

-- | Pretty printing of polynomials.
instance Pretty (Polynomial Double) where
    pPrint p = printPolynomial "x" (map expandTerm (terms p))

-- | Expand term to (Double, MI.MultiIndex)-form suitable for printing.
expandTerm :: Term Double -> (Double, MI.MultiIndex)
expandTerm (Constant c) = (c, MI.zero 0)
expandTerm (Term c mi) = (c ,mi)

\end{code}

%------------------------------------------------------------------------------%

Up until now, the definition of the polynomial type is independent of the
 underlying scalar type. However, computing the directional derivative of a
 polynomial requires extracting the components of a vector which is not provided
 by the \code{Vectorspace} class yet. To make \code{Polynomial} an instance
 of the \code{Function} class it is thus necessary to refrain to a concrete
 type. The implementation of derviation, evaluation and integration is described
 below.

%------------------------------------------------------------------------------%

\begin{code}

-- | Polynomials as functions.
instance S.Function (Polynomial Double) Vector where
  type Values (Polynomial Double) Vector = Double
  type GeomUnit (Polynomial Double) Vector = Simplex
  integrate = integratePolynomial
  deriv = derivePolynomial deriveMonomial
  eval = evaluatePolynomial evaluateMonomial

\end{code}

%------------------------------------------------------------------------------%

For the extension of polynomials defined on simplices it is necessary to extract
 the multi-indices defining the polynomial. To this end the \module{Polynomial}
 module provides the function \code{multiIndices} which converts all of the
 constants in the polynomials to multi-index representation, i.e. with
 multi-index conatining only zeros, and returns a list of them.

%------------------------------------------------------------------------------%

\begin{code}

-- | Returns a list of the multi-indices in the polynomial.
multiIndices :: Int -> Polynomial a -> [MI.MultiIndex]
multiIndices n (Polynomial _ ls) = multiIndices' n ls

multiIndices' :: Int -> [Term a] -> [MI.MultiIndex]
multiIndices' n (Term _ mi  : ls) = mi : multiIndices' n ls
multiIndices' n (Constant _ : ls) = MI.zero n : multiIndices' n ls
multiIndices' _ [] = []

\end{code}

%------------------------------------------------------------------------------%

For the integrateion of Bernstein polynomials it is also necessary to access the
degrees of each term of the polynomial. To keep the internal structure of the
polynomials encapsulated, the function \code{degrees} is provided, that returns
a list containing the degrees of the terms in the polynomial.

%------------------------------------------------------------------------------%

\begin{code}

-- | Return a list of the degrees of each term in the polynomial.
degrees :: Polynomial a -> [Int]
degrees (Polynomial _ ts) = degrees' ts

degrees' :: [Term a] -> [Int]
degrees' ((Term _ mi):ls) = (MI.degree mi) : (degrees' ls)
degrees' ((Constant c):ls) = 0 : (degrees' ls)
degrees' [] = []

\end{code}

%------------------------------------------------------------------------------%


\subsection{Constructors}

The \module{Polynomial} provides constructors for the convenient construction of
 valid polynomials. The stress here lies on valid, because not every instance of
 \code{Polynomial} represents a valid polynomial. For a polynomials to be valid,
 all multi-indices representing the monomials must be of the same langth.

The functions \code{constant}, \code{monomial}, \code{term} and
 \code{polynomial} can be used to create a constant polynomial, a monomial,
 a term (scaled monomial) and a polynomial. The function \code{polynomial}
 performs a check if the given parameters represent a valid polynomial and puts
 out an error message if not.

\begin{code}

-- | Create a constant polynomial with the given value.
constant :: a -> Polynomial a
constant c = Polynomial 0 [Constant c]

-- | Create a polynomial consisting of a single monomial from a give
-- | multi-index.
monomial :: Ring a => MI.MultiIndex -> Polynomial a
monomial mi = Polynomial (MI.degree mi) [Term mulId mi]

-- | Create a term of a polynomial consisting of a scaled monomial.
term :: Ring a => (a, MI.MultiIndex) -> Term a
term (c, mi) = Term c mi

-- | Create a polynomial from a list of coefficient-multi-index pairs.
polynomial :: Ring a => [(a, MI.MultiIndex)] -> Polynomial a
polynomial l = if (checkPolynomial l)
               then Polynomial r (map term l)
               else error "Given coefficients and multi-indices do not define a valid polynomial."
    where r = maximum (map (MI.degree . snd) l)

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

\end{code}

%------------------------------------------------------------------------------%

We also provide a constructor for homogeneous, linear polynomials
\code{linearPolynomial} in $\R{n}$, which takes a list of $n$ coefficients that
contains the slope of the $n$ variables of the polynomials.

%------------------------------------------------------------------------------%

\begin{code}

-- | Create 1st degree homogeneous polynomial in n variables from
-- | length n list of coefficients. The coefficient with index i in the list
-- | equals the coefficient of the ith variable of the returned polynomial.
linearPolynomial :: [a] -> Polynomial a
linearPolynomial l = Polynomial 1 $ zipWith Term l [MI.one n i | i <- [0..n - 1]]
  where n  = length l

\end{code}

%------------------------------------------------------------------------------%

\subsection{Arithmetic Operations}

As mentioned above, polynomials over $\R{n}$ form a ring and a vector space. The
 arithmetic operations we need to implement are thus addition, substraction,
 multiplication and multiplication by a scalar.

Since polynomials are represented as a sum of terms addition of polynomials can
 be implemented by simply concatenating the two lists representing the two
 polynomials.

%------------------------------------------------------------------------------%

\begin{code}

-- | Add two polynomials.
addP :: (Ring a) => Polynomial a -> Polynomial a -> Polynomial a
addP (Polynomial r1 ts1) (Polynomial r2 ts2) =
    Polynomial (max r1 r2) (ts1 ++ ts2)

\end{code}

%------------------------------------------------------------------------------%

The implementation of scaling and multiplication of polynomials is straight
 forward. Scaling of a polynomials amounts to simply scaling each of the terms
 in the polynomials, whereas multiplication of polynomial amounts to multiplying
 each term in the first polynomials with each term in the second polynomial.

%------------------------------------------------------------------------------%

\begin{code}

-- | Scale term by a scalar.
sclTerm :: Ring a => a -> Term a -> Term a
sclTerm c1 (Term c2 mi)  = Term (mul c1 c2) mi
sclTerm c1 (Constant c2) = Constant (mul c1 c2)

-- | Scaling of a polynomial.
sclP :: (Ring a) => a -> Polynomial a -> Polynomial a
sclP c (Polynomial r ts) = Polynomial r (map (sclTerm c) ts)

-- | Multiply two terms.
mulTerm :: Ring a => Term a -> Term a -> Term a
mulTerm (Term c1 mi1) (Term c2 mi2) = Term (mul c1 c2) (MI.add mi1 mi2)
mulTerm (Term c1 mi) (Constant c2) = Term (mul c1 c2) mi
mulTerm (Constant c2) (Term c1 mi) = Term (mul c1 c2) mi
mulTerm (Constant c1) (Constant c2) = Constant (mul c1 c2)

-- | Polynomial multiplication.
mulP :: (Ring a) => Polynomial a -> Polynomial a -> Polynomial a
mulP (Polynomial r1 ts1) (Polynomial r2 ts2) =
    Polynomial (r1 + r2) [mulTerm t1 t2 | t1 <- ts1, t2 <- ts2]
\end{code}

%------------------------------------------------------------------------------%

\subsection{Evaluation}

As mentioned above, the \code{Polynomial} type also provides abstract functions
 for the implementation of polynomials over different bases. To this end, the
\code{evaluateTerm} function provides abstract evaluation of a term. It takes a
 function that evaluates a monomial represented by a multi-index at a given
 vector and evaluates the term at the vector.

The function \code{evaluateMonomial} implements the evaluation function for the
 standard monomial basis over $\R{n}$.

%------------------------------------------------------------------------------%

\begin{code}

-- | General evaluation of a term. Given a function for the evaluation a
-- | monomial, the function returns the corresponding value of the polynomial
-- | scaled by the terms coefficient or simply the value of the term if the term
-- | is constant.
evaluateTerm :: VectorSpace v
         => (v -> MI.MultiIndex -> Scalar v) -- The evaluation function
         -> v                                -- The evaluation vector
         -> Term (Scalar v)                  -- The term to be evaluated
         -> Scalar v
evaluateTerm f v (Term c mi)  = mul c (f v mi)
evaluateTerm _ _ (Constant c) = c

-- | Evaluate monomial over standard monomial basis.
evaluateMonomial :: Vector -> MI.MultiIndex -> Double
evaluateMonomial = V.pow
\end{code}

%------------------------------------------------------------------------------%

General evaluation of a polynomial can then be implemented by using the general
 \code{evalTerm} function to evaluate each term and add up the results. This is
 implemented in the \code{evaluate'} function.

The evaluation of polynomials over the monomial basis can now be realized by
 partially evaluating \code{evaluate} with \code{evalMonomial}.

%------------------------------------------------------------------------------%

\begin{code}

-- | General evaluation function of a polynomial using the given function for
-- | the evaluation of monomials.
evaluatePolynomial :: (Vector -> MI.MultiIndex -> Double) -> Vector -> Polynomial Double -> Double
evaluatePolynomial f v p = foldl add addId (map (evaluateTerm f v) ts)
    where ts = terms p

\end{code}

%------------------------------------------------------------------------------%

\subsection{Derivation}

To provide generalized functions for the derivation of polynomials, we introduce
 the \code{Dx} type as a synomym for functions implementing derivation of
 monomials $\vec{y}^{\vec{alpha}}$. The directional derivative along the $i$th
 dimension of such a monomial is given by

\begin{align}
  \frac{d}{dx_i}\vec{y}^{\vec{alpha}} &= \sum_j \alpha_j \frac{dy_j}{dx_i}\vec{y}^{\vec{\alpha}^j}
\end{align}
where $\vec{\alpha}^j$ is the multi-index $\vec{\alpha}$ with the $j$th index
 decreased by one. The directional derivative of a monomial thus is a polynomial
 consisting of a sum of multiple terms. Here we use a list of \code{Term Double}
 to represent the sum.

The function \code{deriveTerm} then computes the directional derivative of
 a given term along a direction given by a vector, by computing the directional
 derivative of the term and scaling it by the corresponding component of the
 vector. The result is a list of Terms representing a polynomial.

The function \code{deriveMonomial} implements the derivative of a monomial for
 the standard monomial basis in $\R{n}$. In this case the derivative of a
 is just

\begin{align}
  \frac{d}{dx_i}\vec{x}^{\vec{\alpha}} &= \alpha_i\vec{x}^{\vec{\alpha}^j}
\end{align}

%------------------------------------------------------------------------------%

\begin{code}
-- | Type synonym for a function to generalize the derivative of a monomial
-- | in a given space direction. Required to generalize the polinomial code to
-- | different bases.
type Dx a = Int -> MI.MultiIndex -> [Term a]

-- | General derivative of a term. Given a function for the derivative of a monomial
-- | in a given space direction, the function computes the derivative of the given
-- | term using the product rule.
deriveTerm :: Dx Double
           -> Vector
           -> Term Double
           -> [Term Double]
deriveTerm dx v (Constant _) = [Constant 0]
deriveTerm dx v (Term c mi)  = concat [map (sclTerm (v' !! i)) (dx i mi) |
                                       i <- [0..n-1],
                                       MI.degree mi > 0]
    where
      v' = toList v
      n = dim v

-- | Derivative of a monomial over the standard monomial basis in given space
-- | direction.
deriveMonomial :: Dx Double
deriveMonomial i mi
    | i < dim mi = [Term c (MI.decrease i mi)]
    | otherwise = error "deriveMonomial: Direction and multi-index have unequal lengths"
  where c  = fromInt (MI.toList mi !! i)

\end{code}

%------------------------------------------------------------------------------%

In the same way as for the evaluation of polynomials, we provide a
 basis-independent implementation of the deriviation and a concrete one for the
 derivation of polynomials over a monomial basis. The derivative of a polynomial
 is obtained by simply concatenating the derivatives of the terms of the
 polynomial.

%------------------------------------------------------------------------------%

\begin{code}

-- | General derivative for a polynomial with arbitrary basis.
derivePolynomial :: Dx Double -> Vector -> Polynomial Double -> Polynomial Double
derivePolynomial dx v p = Polynomial (r - 1) (concatMap (deriveTerm dx v) ts)
    where r = degree p
          ts = terms p

\end{code}

%------------------------------------------------------------------------------%


\subsection{Integration}

For integration of polynomials over simplices, we use the
\code{integrateOverSimplex} function provided by the \module{Simplex} module.
Since the method presented in \cite{Ainsworth} has precision $2q - 1$, the
integration of a polynomial of degree $r$ is exact if
$q \geq \frac{(r + 1)}{2}$.

The \module{Polynomial} module does not provide a general function for
integration of polynomials because the \code{integrateOverSimplex} functions
already provides general integration of any function over simplices.

%------------------------------------------------------------------------------%

\begin{code}

-- | Numerically integrate the function f over the simplex t using a Gauss-Jacobi
-- | quadrature rule of degree k.
integratePolynomial :: Simplex -> Polynomial Double -> Double
integratePolynomial t p = integrateOverSimplex q t p
    where q = div (r + 2) 2
          r = degree p

\end{code}

%------------------------------------------------------------------------------%

\subsection{Barycentric Coordinates}

One may view barycentric coordinates as a representation of points in $\R{n}$
the simplex as described in section \ref{sec:Coordinates}. Here, however, we
will adopt a different view. Instead of considering the barycentric coordinates
\lambda_0,\ldots,\lambda_n as coordinates of a point $\vec{x} \in \R{n}$, we
consider them as linear functions of $\vec{x}$:

\begin{align}
  \lambda_0(\vec{x}),\ldots,\lambda_n(\vec{x})
\end{align}

For points $\vec{x}$ inside the simplex $T$, the same properties hold as
mentioned in \ref{sec:Coordinates}, namely that

\begin{itemize}
  \item the barycentric coordinates sum to one: $\sum_i \lambda_i(\vec{x}) = 1$
  \item all barycentric coordinates are positive: $\lambda_i(\vec{x}) \geq 0,
    i = 0,\ldots,n$
\end{itemize}

The property defining the barycentric coordinates is that, the barycentric
coordinate $\lambda_i$ is the linear function $\lambda_i(\vec{x})$ that
satisfies

\begin{align}\label{eq:barycentric_prop}
  \lambda_i(\vec{v}_j) = \delta_{ij}
\end{align}

,where $\vec{v}_j$ are the vertices of the simplex $T$ and $\delta_{ij}$ is the
Kronecker delta. Equation (\ref{eq:barycentric_prop}) can be used to determine
the barycentric coordinates of a given full simplex $T$ with vertices
$\vec{v}_0,\ldots,\vec{v}_n$ by solving the resulting linear system of
equations. The linear system of equation results in a $n+1 \times n+1$ matrix
 $\vec{A}$ of the form

\begin{align}
  \vec{A} &= \left [ \begin{array}{cccc}
      1      & \textrm{---} & \vec{v}_0 & \textrm{---} \\ % not beautiful,
      \vdots & \vdots       & \vdots    & \vdots    \\
      1      & \textrm{---} & \vec{v}_n & \textrm{---}    % ... but works
      \end{array} \right ]
\end{align}

 that has to be inverted since the right-hand side vectors are just the $n+1$
unit vectors in $\R{n+1}$. The resulting inverse $\vec{A}^{-1}$ has the form

\begin{align}
  \vec{A} &= \left [ \begin{array}{ccc}
      a_0       & \ldots  & a_n      \\
      |         & \ldots  & |        \\
      \vec{b}_0 & \ldots  & \vec{b_n}\\
      |         & \ldots   & |        \\
      \end{array} \right ]
\end{align}

The barycentric coordinates $\lambda_i(\vec{x})$ are then given by

\begin{align}\label{eq:def_barycentric}
  \lambda_i(\vec{x}) &= a_i + \vec{b}_i^T \vec{x}
\end{align}

Note that the above method assumes that the simplex $T$ has $n+1$ vertices, for
simplices with less vertices it is necessary to extend the simplex first. This
is done using the \code{extendSimplex} function.

The inversion of the matrix is performed using the \code{hlinear} package
\cite{Ruiz}. The \code{SimplexToMatrix} and \code{vectorToPolynomial} functions
handle the conversion between the simplex, the linear system and the polynomials
representing the barycentric coordinates.

Finally, the function \code{barycentricCoordinates} and
\code{barycentricCoordinate} return a list containing all barycentric
coordinates and only the $i$ barycentric coordinate, respectively.

%------------------------------------------------------------------------------%

\begin{code}

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

-- Transforms a given simplex into the matrix representing the linear
-- equation system for the barycentric coordinates.
simplexToMatrix :: Simplex -> M.Matrix Double
simplexToMatrix s@(Simplex _ l) = M.matrix (n+1) (concatMap append1 l)
    where n = geometricalDimension s
          append1 p = 1 : toList (fromPoint p)

-- Transforms a solution vector of the linear equation system for the
-- barycentric coordinates into the corresponding polynomial.
vectorToPolynomial :: M.Vector Double -> Polynomial Double
vectorToPolynomial v = add (constant (head l)) (linearPolynomial (tail l))
    where l = M.toList v

\end{code}

%------------------------------------------------------------------------------%

Since the barycentric coordinates are linear functions, the gradients of the
barycentric coordinates are constant vectors, namely the vectors $\vec{b}_n$ in
equation (\ref{eq:def_barycentric}). The functions \code{barycentricGradients}
and \code{barycentricGradient} return the list of the gradients of the
barycentric coordinates and the gradient of the $i$th barycentric coordinate,
repsectively.

%------------------------------------------------------------------------------%

\begin{code}

-- Transforms a solution vector of the linear equation system into the
-- gradients of the barycentric coordinates.
vectorToGradient :: M.Vector Double -> [Double]
vectorToGradient = tail . M.toList

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

\end{code}
