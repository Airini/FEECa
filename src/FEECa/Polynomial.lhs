\section{Polynomials}

The \module{Polynomial} module provides data types and functions for
polynomials over $\R{n}$.
%
Given a vector $\vec{x} \in \R{n}$, the power of $\vec{x}$ with
respect to a multi-index $\vec{\alpha} =
(\alpha_0,\ldots,\alpha_{n-1})$ is given by

$$
\begin{align}
  \vec{x}^{\vec{\alpha}} &= \prod_{i=0}^{n-1} x_i^{\alpha_i}
 \label{eq:pow}
\end{align}
$$

A polynomial in $\R{n}$ is a function that maps a vector $\vec{x}$ to a
 linear combination of powers of $\vec{x}$:

%TODO: give type of \vec{\alpha} and perhaps rename it to avoid confusion (because $A_i : \{0 \ldots n-1\} \to ℕ $)
$A : I \to (\{0 \ldots n-1\} \to ℕ)$
$$
\begin{align}
  p(\vec{x}) &= \sum_i c_i \vec{x}^{A_i}
\end{align}
$$

Apart from implementing polynomials over vectors in $\R{n}$, the
\module{Polynomial} module also provides abstract functions for
polynomials that use different bases, such as the Bernstein
polynomials implemented in the \module{Bernstein} module.

%------------------------------------------------------------------------------%

\begin{code}

{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module FEECa.Polynomial (

  -- * Polynomial types
    Polynomial (..), Term (..)

  -- ** Constructors
  , polynomial, constant, linearPolynomial, term

  -- ** Manipulation operations
  , expandTerm, monomial, multiIndices, degrees, toPairs

  -- * Mathematical operations
  , evaluatePolynomial, derivePolynomial, integratePolynomial, multiplyPolynomial

  -- * Barycentric Coordinates
  , barycentricCoordinate, barycentricCoordinates
  , barycentricGradient, barycentricGradients
  , barycentricGradients', simplexToMatrix, euclideanToBarycentric

  -- XXX:
  , simplifyT, simplifyP

  ) where


import            Data.List
import qualified  Numeric.LinearAlgebra.HMatrix as M

import            FEECa.Utility.Print   ( Pretty (..), printPolynomial )
import            FEECa.Utility.Utility ( takeMap, zipWithWhen, sumR, productR )

import qualified  FEECa.Internal.MultiIndex as MI (
                      MultiIndex, zero, unit, decrease,
                      toList, add, degree, valid )
import            FEECa.Internal.Simplex
import            FEECa.Internal.Spaces           (
                      Ring (..), Field (..), Dimensioned (..),
                      Module (..), VectorSpace, EuclideanSpace (..),
                      toDouble', fromDouble' )
import qualified  FEECa.Internal.Spaces     as S  ( Function (..) )


\end{code}

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

coefficient :: Term a -> a
coefficient (Constant a) = a
coefficient (Term a _)   = a

instance Functor Term where
  fmap f (Constant a) = Constant (f a)
  fmap f (Term a mi)  = Term (f a) mi
\end{code}

%------------------------------------------------------------------------------%

Based on the \code{Term} type, a polynomial is now be represented as a list of
 terms, keeping the sum implicit. In addition to that, the \code{Polynomial}
type provides the \code{degree} field that holds the degree of the polynomial.

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

instance Functor Polynomial where
  fmap f (Polynomial n ts) = Polynomial n (map (fmap f) ts)
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
multiIndices' _ []                = []

\end{code}

%------------------------------------------------------------------------------%

For the integration of Bernstein polynomials it is also necessary to access the
degrees of each term of the polynomial. To keep the internal structure of the
polynomials encapsulated, the function \code{degrees} is provided, that returns
a list containing the degrees of the terms in the polynomial.

%------------------------------------------------------------------------------%

\begin{code}

-- | Return a list of the degrees of each term in the polynomial.
degrees :: Polynomial a -> [Int]
degrees (Polynomial _ ts) = degrees' ts

degrees' :: [Term a] -> [Int]
degrees' []              = []
degrees' (Term  _ mi:ls) = MI.degree mi : degrees' ls
degrees' (Constant _:ls) = 0 : degrees' ls

\end{code}

%------------------------------------------------------------------------------%

For some computations on Bernstein polynomials it is necessary to obtain the
polynomial represented as a list of coefficient-multi-index pairs. To this end
the \code{toPairs} function is provided, that extends all constant terms in the
polynomial and returns a list of the coefficient-multi-index pairs.

%------------------------------------------------------------------------------%

\begin{code}

-- | Return polynomial represented as list of coefficient-multi-index pairs.
toPairs :: Int -> Polynomial a -> [(a, MI.MultiIndex)]
toPairs n = map (expandTerm n) . terms

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Class Instantiations}

Polynomials over $\R{n}$ form a vector space over $\mathrm R$ and a ring. To
 make their algebraic structure available in \code{FEECa}, we make them instances
 of the \code{Vectorspace} and \code{Ring} classes. The implementation of the
 arithmetic functions on polynomials is given below.

%------------------------------------------------------------------------------%

\begin{code}

-- | Polynomials as vector spaces.
instance Ring a => Module (Polynomial a) where
  type Scalar (Polynomial a) = a
  addV = addPolynomial
  sclV = scalePolynomial

instance Field a => VectorSpace (Polynomial a)

-- | Polynomials as a ring.
instance Ring a => Ring (Polynomial a) where
  add    = addV
  addId  = constant addId
  addInv = sclV (addInv mulId)

  mul    = multiplyPolynomial multiplyMonomial
  mulId  = constant mulId

  embedIntegral = constant . embedIntegral

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
  pPrint p = printPolynomial "x" (map (expandTerm 0) (terms p))

-- | Expand term to (Double, MI.MultiIndex)-form suitable for printing.
expandTerm :: Int -> Term a -> (a, MI.MultiIndex)
expandTerm n (Constant c) = (c, MI.zero n)
expandTerm _ (Term c mi)  = (c, mi)

\end{code}

%------------------------------------------------------------------------------%

 The main purpose of the polynomial type is of course its use as a function over
 $\R{n}$. We therefore declare the type an instance of the abstract
 \code{Function} class. The implementation of evaluation and derivation of
 polynomials is given below.
%------------------------------------------------------------------------------%

\begin{code}

-- | Polynomials as functions.

instance (EuclideanSpace v, r ~ Scalar v) => S.Function (Polynomial r) v where
  evaluate = evaluatePolynomial . evaluateMonomial . toList
  derive   = derivePolynomial deriveMonomial

\end{code}

\subsubsection{Constructors}

The \module{Polynomial} provides smart constructors to simplify the creation of
 valid polynomials. The stress here lies on valid, because not every instance of
 \code{Polynomial} represents a valid polynomial. For a polynomials to be valid,
 all multi-indices representing the monomials must be of the same length.

The functions \code{constant}, \code{monomial}, \code{term} and
 \code{polynomial} can be used to create a constant polynomial, a monomial,
 a term (scaled monomial) and a polynomial. The function \code{polynomial}
 performs a check if the given parameters represent a valid polynomial and puts
 out an error message if not.

\begin{code}

-- | Create a constant polynomial with the given value.
constant :: a -> Polynomial a
constant c = Polynomial 0 [Constant c]

-- | Create a polynomial consisting of a single monomial from a given
-- | multi-index.
monomial :: Ring a => MI.MultiIndex -> Polynomial a
monomial mi = Polynomial (MI.degree mi) [Term mulId mi]

-- | Create a term of a polynomial consisting of a scaled monomial.
term :: Ring a => (a, MI.MultiIndex) -> Term a
term = uncurry Term

-- | Create a polynomial from a list of coefficient-multi-index pairs.
polynomial :: Ring a => [(a, MI.MultiIndex)] -> Polynomial a
polynomial l
    | checkPolynomial l = Polynomial r $ aggregate (map term l)
    | otherwise         = error "Given coefficients and multi-indices do not define a valid polynomial."
  where r = if null l then 0 else maximum (map (MI.degree . snd) l)

-- | Check whether a list of coefficient-multi-index pairs represents a
-- | polynomial.
checkPolynomial :: [(a, MI.MultiIndex)] -> Bool
checkPolynomial ls = all (MI.valid . snd) ls && sameLength (map snd ls)

-- | Check if all multi-indices in the list have the same dimension.
sameLength :: [MI.MultiIndex] -> Bool
sameLength []     = True
sameLength (l:ls) = all ((== dim l) . dim) ls

{-sameLength' :: Int -> [MI.MultiIndex] -> Bool
sameLength' i (l:ls)  = (i == dim l) && sameLength' (dim l) ls
sameLength' _ []      = True-}

\end{code}

%------------------------------------------------------------------------------%

In addition to that a constructor for homogeneous, linear polynomials
\code{linearPolynomial} in $\R{n}$ is provided, which takes a list of $n$
 coefficients that contains the slope of the $n$ variables of the polynomials.

%------------------------------------------------------------------------------%

\begin{code}

-- | Create 1st degree homogeneous polynomial in n variables from
-- | length n list of coefficients. The coefficient with index i in the list
-- | equals the coefficient of the ith variable of the returned polynomial.
linearPolynomial :: Ring a => [a] -> Polynomial a
linearPolynomial l = Polynomial 1 $ zipWithWhen Term (flip (const (/= addId)))
                                                l [MI.unit n i | i <- [0..n - 1]]
  where n  = length l
-- TODO: check so as to have a smarter constructor

\end{code}

%------------------------------------------------------------------------------%

\subsection{Arithmetic}

As mentioned above, polynomials over $\R{n}$ form a ring and a vector space. The
 arithmetic operations we need to implement are thus addition, substraction,
 multiplication and multiplication by a scalar.

\subsubsection{Addition}
Since polynomials are represented as a sum of terms addition of polynomials can
 be implemented by simply concatenating the two lists representing the two
 polynomials.

%------------------------------------------------------------------------------%

\begin{code}

-- | Add two polynomials.
addPolynomial :: Ring a => Polynomial a -> Polynomial a -> Polynomial a
addPolynomial (Polynomial r1 ts1) (Polynomial r2 ts2)
    | null ts   = constant addId
    | otherwise = Polynomial (max r1 r2) ts
  where ts = {-(aggregate . removeZeros)-} aggregate (ts1 ++ ts2)

--removeZeroes :: Ring a => [Term a] -> [Term a]
--removeZeroes = filter ((/= addId) . coefficient)
\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Scaling and Multiplication}

The implementation of scaling and multiplication of polynomials is straight
 forward. Scaling of a polynomials amounts to simply scaling each of the terms
 in the polynomials, whereas multiplication of polynomial amounts to multiplying
 each term in the first polynomials with each term in the second polynomial.

%------------------------------------------------------------------------------%

\begin{code}

-- | Scale term by a scalar.
scaleTerm :: Ring a => a -> Term a -> Term a
scaleTerm = fmap . mul

-- | Scaling of a polynomial.
scalePolynomial :: Ring a => a -> Polynomial a -> Polynomial a
scalePolynomial c | c == addId = const addId
                  | otherwise  = fmap (mul c)
-- c (Polynomial r ts) = Polynomial r (map (scaleTerm c) ts)

-- | Multiplication of two terms of a given function for monomial
-- | multiplication when represented as multi-indices.
multiplyTerm :: Ring a
             => (MI.MultiIndex -> MI.MultiIndex -> Term a)
             -> Term a -> Term a -> Term a
multiplyTerm _ (Constant c)  t             = scaleTerm c t -- (Constant c2) = Constant (mul c1 c2)
--multiplyTerm _ (Constant c)  t(Term c2 mi)  = Term (mul c1 c2) mi
multiplyTerm _ t             (Constant c)  = scaleTerm c t --Term (mul c1 c2) mi
multiplyTerm f (Term c1 mi1) (Term c2 mi2) = scaleTerm (mul c1 c2) (f mi1 mi2)

-- | Multiplication of two monomials.
multiplyMonomial :: Ring a => MI.MultiIndex -> MI.MultiIndex -> Term a
multiplyMonomial mi1 mi2
  | dim mi1 == dim mi2  = Term mulId (MI.add mi1 mi2)
  | otherwise = error "multiplyMonomial: Polynomial dimensions don't agree."

-- | General multiplication of polynomials in terms of a given function for
-- | monomial multiplication.
multiplyPolynomial :: Ring a
                   => (MI.MultiIndex -> MI.MultiIndex -> Term a)
                   -> Polynomial a -> Polynomial a -> Polynomial a
multiplyPolynomial f (Polynomial r1 ts1) (Polynomial r2 ts2) =
  Polynomial (r1 + r2) $ aggregate [multiplyTerm f t1 t2 | t1 <- ts1, t2 <- ts2]
\end{code}

%------------------------------------------------------------------------------%

\subsection{Evaluation}

 The \code{Polynomial} type also provides abstract functions for the
 implementation of polynomials over different bases. To this end, the
\code{evaluateTerm} function provides an abstract function for the evaluation of
 a term of a polynomial, which takes a function that evaluates a monomial given
 by a given multi-index and scales the result be the terms coefficient.

The function \code{evaluateMonomial} implements the evaluation function for the
 standard monomial basis over $\R{n}$. In this case the evaluation function for
 the monomial just implements eq. $\eqref{eq:pow}$.

%------------------------------------------------------------------------------%

\begin{code}
-- | General evaluation of a term. Given a function for the evaluation a
-- | monomial, the function returns the corresponding value of the polynomial
-- | scaled by the terms coefficient or simply the value of the term if the term
-- | is constant.
evaluateTerm :: Ring r
             => (MI.MultiIndex -> r) -> Term r -> r
evaluateTerm f (Term c mi)  = mul c (f mi)
evaluateTerm _ (Constant c) = c

-- | Evaluate monomial over standard monomial basis.
evaluateMonomial :: Ring r -- (EuclideanSpace v, r ~ Scalar v)
                 => [r] -> MI.MultiIndex -> r
evaluateMonomial v mi = productR (zipWith pow v (MI.toList mi::[Int]))
--  where prod' = foldl mul mulId
\end{code}

%------------------------------------------------------------------------------%

General evaluation of a polynomial can then be implemented by using the general
 \code{evalTerm} function to evaluate each term and add up the results. This is
 implemented in the \code{evaluate'} function.

The evaluation of polynomials over the monomial basis can now be realized by
 partially evaluating \code{evaluate} with \code{evaluateMonomial}.

%------------------------------------------------------------------------------%

\begin{code}

-- | General evaluation function of a polynomial p using the given function f
-- | for the evaluation of monomials.
evaluatePolynomial :: Ring r
                   => (MI.MultiIndex -> r) -> Polynomial r -> r
evaluatePolynomial f = sumR . map (evaluateTerm f) . terms

\end{code}

%------------------------------------------------------------------------------%

\subsection{Derivation}

The derivative of a polynomial is just the sum of the scaled derivatives of the
monomials in each term. A general monomial may be written as a vector valued
function $\vec{y(\vec{x})}$ taken to the power $\vec{\alpha}$ for a given
multi-index $\vec{\alpha}$. The directional derivative along the $i$th dimension of
 such a monomial is given by

\begin{align}
  \frac{d}{dx_i}\vec{y}^{\vec{\alpha}} &= \sum_j \alpha_j \frac{dy_j}{dx_i}\vec{y}^{\vec{\alpha}^j}
\end{align}

where $\vec{\alpha}^j$ is the multi-index $\vec{\alpha}$ with the $j$th index
 decreased by one. The directional derivative of a monomial is thus a polynomial
 consisting of multiple terms.

To provide generalized functions for the derivation of polynomials, we introduce
 the \code{Dx} type as a synomym for functions implementing derivation of
 monomials $\vec{y}^{\vec{\alpha}}$. The returned list is supposed to hold the
 directional derivatives of the given monomial in each space direction.

%------------------------------------------------------------------------------%

\begin{code}
-- | Type synonym for a function to generalize the derivative of a monomial
-- | in a given space direction. Required to generalize the polinomial code to
-- | different bases.
type Dx a = MI.MultiIndex -> [Polynomial a]

\end{code}

%------------------------------------------------------------------------------%

 The function \code{deriveTerm} then computes the directional derivative of
 a given term along a direction given by an arbitrary vector $\vec{v}$, which
 is given by

\begin{align}
  \frac{d\: cvec{y}^{\vec{\alpha}}}{d\vec{v}} &= c\sum_{i = 0}^{n-1} v_{i}
                                              \frac{d\vec{y}}{dx_i}
\end{align}

, by computing the directional
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

-- | General derivative of a term. Given a function for the derivative of a monomial
-- | in a given space direction, the function computes the derivative of the given
-- | term using the product rule.
deriveTerm :: (EuclideanSpace v, r ~ Scalar v)
           => Dx r -> v -> Term r -> Polynomial r
deriveTerm _  _ (Constant _) = constant addId
deriveTerm dx v (Term c mi)  = {-sclV c -} sumR (zipWith (sclV . mul c) v' (dx mi))
  where v' = toList v

-- | Derivative of a monomial over the standard monomial basis in given space
-- | direction.
deriveMonomial :: Ring r => Dx r
deriveMonomial mi = [ polynomial [(c i, mi' i)] | i <- [0..n-1] ]
  where c i   = embedIntegral (MI.toList mi !! i)
        mi' i = MI.decrease i mi
        n     = dim mi
\end{code}

%------------------------------------------------------------------------------%

 In the same way as for the evaluation of polynomials, we provide a
 basis-independent implementation of the deriviation for the derivation of
 polynomials. The function for deriving a polynomial over the monomial basis
 can be obtained by partially evaluating \code{derivePolynomial deriveMonomial}.

%------------------------------------------------------------------------------%

\begin{code}

-- | General derivative for a polynomial with arbitrary basis.
derivePolynomial :: (EuclideanSpace v, r ~ Scalar v)
                 => Dx r -> v -> Polynomial r -> Polynomial r
derivePolynomial dx v = sumR . map (deriveTerm dx v) . terms -- t | t <- terms p ]

\end{code}

%------------------------------------------------------------------------------%


\subsection{Integration}

For integration of polynomials over simplices, we use the
\code{integrateOverSimplex} function provided by the \module{Simplex} module.
Since the method used  has precision $2q - 1$, the integration of a polynomial
 of degree $r$ is exact if $q \geq \frac{(r + 1)}{2}$.

%------------------------------------------------------------------------------%

\begin{code}

-- | Numerically integrate the polynomial p over the simplex t using a Gauss-Jacobi
-- | quadrature rule.
integratePolynomial :: (EuclideanSpace v, r ~ Scalar v)
                    => Simplex v -> Polynomial r -> r
integratePolynomial t p = integrateOverSimplex q t (p S.$$)
  where q = div (r + 2) 2
        r = degree p

\end{code}

%------------------------------------------------------------------------------%

\subsection{Barycentric Coordinates}

An important building block for the construction of Bernstein polynomials are
barycentric coordinates. Barycentric coordinates defined with respect to a
simplex $\smp{T} = [\vec{v}_0,\ldots,\vec{v}_n]$, are a set of $n+1$ coordinates
 $\lambda_0,\ldots,\lambda_n$ satisfying

$$
\begin{align}
  (\lambda_0,\ldots,\lambda_n) = v_i \text{ if }
\lambda_0=\ldots=\lambda_{i-1}=\lambda_{i+1}=\ldots=
\lambda_n = 0 \text{ and } \lambda_i= 1
\end{align}
$$

Viewed as a linear functions over $\R{n}$ the above requriement for the barycentric
coodinates may also be written as

$$
\begin{align}\label{eq:barycentric_prop}
  \lambda_i(\vec{v}_j) = \delta_{ij}
\end{align}
$$

 $\delta_{ij}$ is the Kronecker delta. Equation $\eqref{eq:barycentric_prop}$ can
 be used to determine the barycentric coordinates of a given full simplex
 $\smp{T}$ with vertices $\vec{v}_0,\ldots,\vec{v}_n$ by solving the resulting
 linear system of equations. The linear system can be written as a matrix
equation with a $n+1 \times n+1$ matrix $\vec{A}$ of the form

\begin{align}
  \vec{A} &= \left [ \begin{array}{cccc}
      1      & \mbox{---} & \vec{v}_0 & \textrm{---} \\ % not beautiful,
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

The inversion of the matrix is performed using the \code{hmatrix} package
\cite{Ruiz}. The \code{simplexToMatrix} and \code{vectorToPolynomial} functions
handle the conversion between the simplex, the linear system and the polynomials
representing the barycentric coordinates.

Finally, the function \code{barycentricCoordinates} and
\code{barycentricCoordinate} return a list containing all barycentric
coordinates and only the $i$ barycentric coordinate, respectively.

%------------------------------------------------------------------------------%

\begin{code}

euclideanToBarycentric :: EuclideanSpace v
                       => Simplex v -> [v] -> [v]
euclideanToBarycentric t vs = map (fromDouble' . M.toList) $ M.toRows res
  where res  = vmat M.<> mat
        mat  = M.inv (simplexToMatrix (extendSimplex t))
        vmat = M.matrix (n+1) $ concatMap ((1.0:) . toDouble') vs
        n    = geometricalDimension t

-- | 1st degree polynomial taking value 1 on vertex n_i of the simplex and
-- | 0 on all others. Requires the topological dimension of the simplex to be
-- | as large as the geometrical dimension, i.e. the simplex must contain n+1
-- | vertices if the underlying space has dimensionality n.
-- TODO: check take
barycentricCoordinates :: (EuclideanSpace v, r ~ Scalar v)
                       => Simplex v -> [ Polynomial r ]
barycentricCoordinates s = {-# SCC "barycentricCoordinates" #-}
    takeMap nt vectorToPolynomial (M.toColumns mat)
  where mat = {-# SCC "solveSystem" #-} M.inv (simplexToMatrix (extendSimplex s))
        nt  = topologicalDimension s

-- | Simple wrapper for barycentricCoordinates that picks out the ith polynomial
-- | in the list
barycentricCoordinate :: (EuclideanSpace v, r ~ Scalar v)
                      => Simplex v -> Int -> Polynomial r
barycentricCoordinate s i = barycentricCoordinates s !! i

-- Transforms a given simplex into the matrix representing the linear
-- equation system for the barycentric coordinates.
simplexToMatrix :: EuclideanSpace v
                => Simplex v -> M.Matrix Double
simplexToMatrix s@(Simplex _ l) = M.matrix (n+1) (concatMap prefix1 l)
  where n         = geometricalDimension s
        prefix1 v = 1 : toDouble' v

-- Transforms a solution vector of the linear equation system for the
-- barycentric coordinates into the corresponding polynomial.
vectorToPolynomial :: Field s
                   => M.Vector Double -> Polynomial s
vectorToPolynomial v = add (constant (head l)) (linearPolynomial (tail l))
  where l = map fromDouble (M.toList v)

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
vectorToGradient :: EuclideanSpace v
                 => M.Vector Double -> v
vectorToGradient = fromDouble' . tail . M.toList

-- | Compute gradients of the barycentric coordinates.
barycentricGradients :: EuclideanSpace v
                     => Simplex v -> [v]
barycentricGradients t = {-# SCC "barycentricGradients" #-}
    takeMap nt vectorToGradient (M.toColumns mat)-- (take (nt+1) (M.toColumns mat))
  where mat = M.inv (simplexToMatrix (extendSimplex t))
        nt  = topologicalDimension t

-- | Compute gradients of the barycentric coordinates.
barycentricGradients' :: EuclideanSpace v
                      => Simplex v -> [v]
barycentricGradients' t = map (fromDouble' . M.toList) (tail (M.toRows mat))
  where mat = M.inv (simplexToMatrix (extendSimplex t))

-- | Compute gradient of the barycentric coordinate corresponding to edge i
barycentricGradient :: EuclideanSpace v
                    => Simplex v
                    -> Int
                    -> v
barycentricGradient t i = barycentricGradients t !! i

\end{code}

\begin{code}
simplifyP :: Ring a => Polynomial a -> Polynomial a
simplifyP (Polynomial d ts) = Polynomial d (aggregate {-filter ((/= addId) . coefficient) ts) -} (map simplifyT ts))

-- combine all Constant terms into one
-- combine all terms of the same ZipList into one
type SimpleTerms a = [Term a]
aggregate :: Ring a => [Term a] -> SimpleTerms a
aggregate = foldr aggStep []
  where aggStep :: Ring a => Term a -> SimpleTerms a -> SimpleTerms a
        aggStep t [] = [t]
        aggStep (Constant c1) (Constant c2 : ts) = Constant (add c1 c2) : ts
        aggStep (Constant c1) ts                 = Constant c1 : ts
        aggStep (Term fa1 mi) ts = if null matches then insertTerm (Term fa1 mi) ts
                                                   -- else insertTerm (Term (add fa1 fa2) mi) rest
                                                   else insertTerm (Term (sumR (fa1:map coefficient matches)) mi) rest
          where -- TODO: check this code
                (matches, rest) = partition (eqMI mi) ts
                -- [Term fa2 _mi'] = matches -- TODO: is matches always of length 1?

eqMI :: MI.MultiIndex -> Term a -> Bool
eqMI _  (Constant _)  =  False -- all (0==) mi
eqMI mi (Term _ mi')  =  mi == mi'

insertTerm :: Ring a => Term a -> SimpleTerms a -> SimpleTerms a
insertTerm t (Constant c : ts)
  | c == addId  = t : ts
  | otherwise   = Constant c : t : ts
insertTerm t ts = t : ts

simplifyT :: Ring a => Term a -> Term a
simplifyT (Term fa _) | fa == addId = Constant addId
simplifyT t                         = t
\end{code}
