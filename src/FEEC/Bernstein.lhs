\section{Bernstein Polynomials}

A barycentric monomial of degree $r$ $\b^r_{\vec{\alpha}}$ defined over a simplex
$\smp{T}=[\vec{v_o},\ldots,\vec{v_k}]$ is a product of the form

\begin{align}
 \b _{\vec{\alpha}}^r(\vec{x}) &= \frac{r!}{\vec{\alpha}!}\prod_{i = 0}^k \lambda_i^{\alpha_i}(\vec{x})
 \end{align}

 where the $\lambda_i$ are the barycentric coordinates with respect to $\smp{T}$.
 The factorial of a multi-index $\vec{\alpha} = (\alpha_0,\ldots,\alpha_k)$ is
 defined as

\begin{align}
  \vec{\alpha} &= \prod_{i = 0}^{k} \alpha_i!
\end{align}

The barycentric polynomials of degree $r$ over a given simplex form a basis of
the polynomials of degree $r$ over $\R{n}$.

%------------------------------------------------------------------------------%

\begin{code}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FEEC.Bernstein where
import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Internal.Simplex
import FEEC.Internal.Spaces

import FEEC.Polynomial ( Polynomial, Term, term, terms, expandTerm,
                        evaluatePolynomial, derivePolynomial, multiplyPolynomial,
                        barycentricCoordinates, barycentricGradient,
                        barycentricGradients', toPairs)
import qualified FEEC.Polynomial as P (degree, multiIndices, monomial,
                                       constant, polynomial)


import FEEC.Utility.Combinatorics(choose, factorial)
import FEEC.Utility.Print
--import FEEC.Utility.Utility(factorial)

\end{code}

%------------------------------------------------------------------------------%

\subsection{The \code{BernsteinPolynomial} Type}

The representation of Bernstein polynomials is generally similar to the one used
for common polynomials. In addition to that the Bernstein polynomial type has
a field \code{simplex} which holds the Simplex over which the polynomial is
defined. In order to be able to create constant polynomials independent of a
simplex, the \code{BernsteinPolynomial} type provides an additional constructor
for constant Bernstein polynomials.

%------------------------------------------------------------------------------%

\begin{code}

-- | Bernstein polynomial over a simplex. Represented by a normal polynomial
-- | internally and uses the generalized functions for evaluation and derivation.
data BernsteinPolynomial v r = Bernstein (Simplex v) (Polynomial r)
                             | Constant r
  deriving (Eq, Show)

-- pretty printing for Bernstein polyonmials
instance Pretty (BernsteinPolynomial v Double) where
{-<<<<<<< HEAD
  pPrint (Bernstein t p) = printPolynomial0 lambda ts
    where ts = map (expandTerm 0) (terms p)
  pPrint (Constant p) = text (show p)
=======-}
    pPrint (Bernstein t p) = printBernstein ts
        where ts = map (expandTerm 0) (terms p)
    pPrint (Constant p) = text (show p)

-- | List multi-indices of the terms in the polynomial.
multiIndices :: EuclideanSpace v r => BernsteinPolynomial v r -> [MI.MultiIndex]
multiIndices (Bernstein t p) = P.multiIndices n p
    where n = geometricalDimension t

degree :: BernsteinPolynomial v r -> Int
degree (Bernstein t p) = P.degree p
degree (Constant _) = 0
\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Constructors}

Since not all possible instances of the type \code{BernsteinPolynomial}
represent valid Bernstein polynomials, the constructors have to make sure that
the constructed polynomials are consistent. To be valid, all multi-indices must
have the same dimension which is just the topological dimension $k$ of the
simplex $T = [\vec{v_0},\ldots,\vec{v_k}]$ plus one. Note that in Bernstein
 representation the multi-indices representing the polynomial are of dimension
 $k+1$, while in the monomial representation they have length $k$.

The function \code{polynomial} creates a Bernstein polynomial from a list of
coefficient-multi-index pairs. The functions throws an error if the provided
arguments are invalid.

The function \code{monomial} creates a barycentric monomial from a given
multi-index and throws an error if dimension of simplex and the multi-index are
inconsisten.

The function \code{constant} creates a constant barycentric monomial and does
not require to be passed a simplex argument.
%------------------------------------------------------------------------------%

\begin{code}

-- | Create a Bernstein polynomial over the given simplex from a list of
-- | coefficient-multi-index pairs. An error is thrown if the dimension of the
-- | multi-indices and the simplex are inconsistent.
polynomial :: EuclideanSpace v r
           => Simplex v
           -> [(r, MI.MultiIndex)]
           -> BernsteinPolynomial v r
polynomial t l
    | (n1 == n2 + 1) && sameLength = Bernstein t (P.polynomial l)
    | otherwise = error "polynomial: Dimensions of Simplex and Polynomials do not match."
{-<<<<<<< HEAD
  where
    mis        = map (dim . snd) l
    n1         = maximum mis
    n2         = topologicalDimension t
    sameLength = all (head mis ==) (tail mis)
=======-}
    where
      mis        = map (((-1)+) . dim . snd) l
      n1         = maximum mis
      n2         = topologicalDimension t
      sameLength = all (head mis ==) (tail mis)


-- | Create a Bernstein monomial over a given simplex from a given
-- | multi-index.
monomial :: EuclideanSpace v r
         => Simplex v -> MI.MultiIndex -> BernsteinPolynomial v r
monomial t mi
    | n1 == n2 + 1 = Bernstein t (P.monomial mi)
    | otherwise   = error "monomial: Dimension of Simplex and Polynomials do not match."
  where n1 = dim mi
        n2 = topologicalDimension t

-- | Create a constant bernstein monomial.
constant :: EuclideanSpace v r => r -> BernsteinPolynomial v r
constant = Constant

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Class Instantiation}


Since the Bernstein polynomials are just one special representation of the
polynomials over $\R{n}$, they have the same algebraic structure. That is they
form a ring with respect to addition and multiplication of polynomials and a
vector space over $\mathrm R$. The algebraic structure is implemented by the
\code{Ring} and \code{VectorSpace} class, respectively.

%------------------------------------------------------------------------------%

\begin{code}

-- | Bernstein polynomials as a vector space.
instance EuclideanSpace v r => VectorSpace (BernsteinPolynomial v r) where
    type Scalar (BernsteinPolynomial v r) = r
    addV = addBernstein
    sclV = scaleBernstein

-- | Bernstein polynomials as a ring.
instance EuclideanSpace v r => Ring (BernsteinPolynomial v r) where
    add = addBernstein
    addId = Constant addId
    addInv = scaleBernstein (sub addId mulId)

    mul = multiplyBernstein
    mulId = Constant mulId

    fromInt = Constant . fromInt
\end{code}

%------------------------------------------------------------------------------%

Moreover, Bernstein polynomials are also functions that can be evaluated and
derived so they are declared an instance of the \code{Function}.

%------------------------------------------------------------------------------%

\begin{code}

instance EuclideanSpace v r => Function (BernsteinPolynomial v r) v  where
  evaluate v (Bernstein t p) = evaluatePolynomial (evaluateMonomial lambda) p
      where lambda = map (evaluate v) (barycentricCoordinates t)
  evaluate v (Constant c) = c
  derive = deriveBernstein

\end{code}

%------------------------------------------------------------------------------%

\subsection{Arithmetic}

Addition and scaling of Bernstein polynomials is straight-forward and can be
implemented using the functions \code{addPolynomial} and \code{scalePolynomial}
provided by the \code{Polynomial} module.

%------------------------------------------------------------------------------%

\begin{code}

-- | Add Bernstein polynomials.
addBernstein :: EuclideanSpace v r
             => BernsteinPolynomial v r
             -> BernsteinPolynomial v r
             -> BernsteinPolynomial v r
addBernstein (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t2 = error "addBernstein: Inconsistent simplices."
     | otherwise = Bernstein t1 (add p1 p2)
addBernstein (Constant c)    (Bernstein t p) = Bernstein t (add p (P.constant c))
addBernstein (Bernstein t p) (Constant c)    = Bernstein t (add p (P.constant c))
addBernstein (Constant c1)   (Constant c2)   = Constant (add c1 c2)


-- | Scale Bernstein polynomial.
scaleBernstein :: Ring r
               => r
               -> BernsteinPolynomial v r
               -> BernsteinPolynomial v r
scaleBernstein c  (Bernstein t p) = Bernstein t (sclV c p)
scaleBernstein c1 (Constant c2)   = Constant (mul c1 c2)

\end{code}
%------------------------------------------------------------------------------%


For the multiplication of Bernstein polynomials one need to take into account
the prefactors of the two polynomials:

% Apparently Mathjax doesn't like binomial coefficients in \frac environments.
$$
\begin{align}
  \b_{\vec{\alpha}_1}^{r_1}\cdot\b_{\vec{\alpha}_2}^{r_2} &=
 \frac%
{\left ( \begin{array}{c} \vec{\alpha}_1 + \vec{\alpha}_2 \\
                 \vec{\alpha}_1 \end{array} \right )}
{\left (\begin{array}{c}r_1 + r_2 \\
          r_1 \end{array} \right )}
  \b_{\vec{\alpha_1}+\vec{\alpha}_2}^{r_1+r_2}
\end{align}
$$

The above equation for the multiplication of two Bernstein monomials is
implemented in \code{multiplyMonomial}. Multiplication of Bernstein polynomials
can then be implemented by partially evaluating
\code{multiplyPolynomials multiplyMonomial}, where \code{multiplyPolynomial} is
the general multiplication function provided by \code{Polynomial} module.

%------------------------------------------------------------------------------%

\begin{code}
multiplyMonomial :: Field r
                    => MI.MultiIndex
                    -> MI.MultiIndex
                    -> Term r
multiplyMonomial mi1 mi2 = term (c, MI.add mi1 mi2)
    where c1 = fromInteger (MI.add mi1 mi2 `MI.choose` mi1)
          c2= fromInteger ((r1 + r2) `choose` r1)
          c  = fromDouble (c1 / c2)
          r1 = MI.degree mi1 :: Integer
          r2 = MI.degree mi2 :: Integer

-- | Multiply two Bernstein polynomials.
multiplyBernstein :: EuclideanSpace v r
                  => BernsteinPolynomial v r
                  -> BernsteinPolynomial v r
                  -> BernsteinPolynomial v r
multiplyBernstein (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t1 = error "multiplyBernstein: Inconsistent simplices."
     | otherwise = Bernstein t1 (multiplyPolynomial multiplyMonomial p1 p2)
multiplyBernstein (Constant c)      (Bernstein t1 p1) = Bernstein t1 (sclV c p1)
multiplyBernstein (Bernstein t1 p1) (Constant c)      = Bernstein t1 (sclV c p1)
multiplyBernstein (Constant c1)     (Constant c2)     = Constant (mul c1 c2)

\end{code}

%------------------------------------------------------------------------------%

\subsection{Evaluation}

For evaluation the general function from the \module{Polynomial} module can be
used, so all that is necessary is to provide a function the evaluates a single
Bernstein monomial. Since the evaluation of each monomial in the polynomial
requires the evaluation of the barycentric coordinates at the given point, the
\code{evalMonomial} function takes the vector containing the values of the $n+1$
barycentric coordinates as additional argument.

%------------------------------------------------------------------------------%

\begin{code}

-- | Evaluate a Bernstein monomial over given
evaluateMonomial :: Field r
                 => [r]
                 -> MI.MultiIndex
                 -> r
evaluateMonomial lambda mi = mul (prefactor r mi) (pow' lambda mi)
    where r = MI.degree mi
          pow' lambda mi = foldl mul mulId (zipWith pow lambda mi')
          mi' = (MI.toList mi) :: [Int]

-- | Prefactor for Bernstein polynomials.
prefactor :: Field r => Int -> MI.MultiIndex -> r
prefactor r a = fromDouble f
    where f = fromInteger (factorial r) / fromInteger (MI.factorial a)


\end{code}

%------------------------------------------------------------------------------%

\subsection{Derivation}

The derivative of a Bernstein monomial along a given direction in space is given
 by

$$
\begin{align}
  \frac{d}{dx_i}\B_{\vec{\alpha}}^{r}
    &= r\sum_{j=0}^n \frac{d\lambda_j}{dx_i}\B_{\vec{\alpha^j}}^{r-1}
\end{align}
$$

where $\vec{\alpha^j}$ is the multi-index $\vec{\alpha}$ with the exponent at
position $j$ decreased by one and $\frac{d\lambda_j}{dx_i}$ is the $i$th
component of the gradient of the $j$th barycentric coordinate. Note that the
factor $\alpha_i$ in the derivation is absorbed into the prefactor
$\frac{(r-1)!}{\vec{\alpha}}$ of $\B_{\vec{\alpha^j}}^{r-1}$.

The derivation of Bernstein monomials is implemented in \code{deriveMonomial}.
The derivation of Bernstein polynomials is then implemented using the general
function provided by the \module{Polynomial} module.

%------------------------------------------------------------------------------%

\begin{code}

-- | Derivative of a Bernstein monomial
deriveMonomial :: EuclideanSpace v r
               => Simplex v
               -> MI.MultiIndex
               -> [ Polynomial r ]
deriveMonomial t mi = [ sum' [sclV (grads i j) (dp j) | j <- [0..n]] | i <- [0..n-1] ]
    where grads i j = (toList ((barycentricGradients' t) !! i)) !! j
          dp j = if ((mi' !! j) > 0)
                 then P.polynomial [(r , MI.decrease j mi)]
                 else P.constant addId
          sum' = foldl add addId
          mi'  = (MI.toList mi) :: [Int]
          n    = (dim mi) - 1
          r    = fromInt( (MI.degree mi) :: Int )
                 
-- | Derive Bernstein polynomial.
deriveBernstein :: EuclideanSpace v r
                => v
                -> BernsteinPolynomial v r
                -> BernsteinPolynomial v r
deriveBernstein v (Bernstein t p) = Bernstein t (derivePolynomial (deriveMonomial t) v p)
deriveBernstein v (Constant c)  = Constant addId
\end{code}

%------------------------------------------------------------------------------%

\subsection{Integration}

 For the integration of Bernstein polynomials two functions are provided.

 The first one uses the quadrature rule over simplices to compute integrals over
 arbitrary simplices. This is implemented by the \code{integrate} function.

%------------------------------------------------------------------------------%

\begin{code}

-- | Numerically integrate the Bernstein polyonomial p over the simplex t using
-- | a Gauss-Jacobi quadrature rule.
integratePolynomial :: EuclideanSpace v (Scalar v)
                    => Simplex v                        -- t
                    -> BernsteinPolynomial v (Scalar v) -- b
                    -> Scalar v
integratePolynomial t b = integrateOverSimplex q t (flip evaluate b)
    where q = div (r + 2) 2
          r = degree b

\end{code}

%------------------------------------------------------------------------------%

The second function computes the integral of a Bernstein polynomial over the
simplex it is defined over, which is given by
\begin{align}
\int_{\smp{T}}\B_{\vec{\alpha}}^{r} \: d\vec{x} &= \frac{[\smp{T}]}%
{\left ( \begin{array}{c} k + d \\ k \end{array} \right )}
\end{align}

where $k$ is the topological dimension of the simplex. Since this requires the
simplex the polynomial is defined over to be known this function does not work
for the \code{Constant} constructor. The \code{redefine} function can be used to
add the information about the simplex to the Bernstein polynomial.

%------------------------------------------------------------------------------%

\begin{code}

-- | Closed-form integration of Bernstein polynomials over the simplex they are
{-<<<<<<< HEAD
-- | defined over. Falls back to standard integration if the provided simplex
-- | does not equal the simplex the bernstein polynomial is defined over. 
-- integrateBernstein :: Simplex -> BernsteinPolynomial -> Double
-- integrateBernstein t1 b@(Bernstein t2 p)
--     | t1 == t2  = sum (map f (toPairs k p))
--     | otherwise = integrate t1 b
--   where f (c, mi) = c * vol / ((k + MI.degree mi) `choose` k)
--         k = topologicalDimension t1
--         vol = volume t1
-- integrateBernstein t (Constant c) = c * (volume t)
=======-}
-- | defined over.
integrateBernstein :: EuclideanSpace v r
                      => BernsteinPolynomial v r
                      -> r
integrateBernstein b@(Bernstein t1 p) = sum' (map f (toPairs k p))
    where f (c, mi) = mul c (divide vol (fac mi))
          fac mi = fromDouble (fromInteger ((k + MI.degree mi) `choose` k))
          k = topologicalDimension t1
          sum' = foldl add addId
          vol = volume t1
integrateBernstein (Constant c) = error "intergrateBernstein: No associated simplex for constant. Define over simplex first using redefine."

-- | Redefined Bernstein polynomial over a different simplex or define simplex
-- | for constant bernstein polynomial.
redefine :: Ring r
         => Simplex v
         -> BernsteinPolynomial v r
         -> BernsteinPolynomial v r
redefine t1 (Bernstein t2 p) = Bernstein t1 p
redefine t (Constant c)      = Bernstein t (P.constant c)

\end{code}

%------------------------------------------------------------------------------%

\subsection{Basis for Differential Forms}

The gradients of the barycentric coordinates

\begin{align}
  d\lambda_i = \left ( \begin{array}{c} \frac{d\lambda_i}{dx_0} \\
                          \vdots \\
                      \frac{d\lambda_i}{dx_{n-1}} \end{array} \right )
\end{align}

of a simplex $\smp{T}$ span the space of alternating one-forms in $\R{n}$. The
projection along the gradient then amounts to simply forming the dot product
with the gradient vector.

%------------------------------------------------------------------------------%

\begin{code}

-- | Projection fuction for gradients of barycentric coordinates as basis for
-- | the space of alternating forms.
proj :: EuclideanSpace v r
     => Simplex v
     -> Int
     -> v
     -> r
proj t i = dot u
    where u = barycentricGradient t i
\end{code}

%------------------------------------------------------------------------------%

\subsection{Extension of Bernstein Polynomials}

The extension of a Bernstein polynomial from a face of a simplex to the simplex
is simply the polynomial that results from extending each of the multi-indices
representing the polynomial to the simplex. For the extension of multi-indices
see \ref{sec:mi_extension}.

%------------------------------------------------------------------------------%

\begin{code}

-- | Extend a Bernstein polynomial defined on a subsimplex f to the simplex t.
extend :: EuclideanSpace v r
       => Simplex v
       -> BernsteinPolynomial v r
       -> BernsteinPolynomial v r
extend t (Bernstein f p) = polynomial t (extend' (toPairs n' p))
    where extend' = map (\(c, mi) -> (c, MI.extend n (sigma f) mi))
          n = topologicalDimension t
          n' = topologicalDimension f
extend _ c = c

\end{code}
