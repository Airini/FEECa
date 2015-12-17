\section{Bernstein Polynomials}

A barycentric monomial $\b^r_{\vec{\alpha}}$ of degree $r$  defined over a simplex
$\smp{T}=[\vec{v_o},\ldots,\vec{v_k}]$ is a product of the form

\begin{align}
 \b _{\vec{\alpha}}^r(\vec{x}) &= \prod_{i = 0}^k \lambda_i^{\alpha_i}(\vec{x})
 \end{align}

 with $|\alpha| = k$ and the $\lambda_i$ the barycentric coordinates
with respect to $\smp{\smp{T}}$.  The space $\ps{r}{\smp{f}}$ of
polynomials of degree at most $r$ in $n$ dimensions defined over a
$k$-dimensional subsimplex $\smp{f}$ of an $n$-dimensional simplex
$\smp{T}$ is isomorphic to the the space of $\mathcal{H}_{r}(\R{k+1})$
of homogeneous polynomials over the $k+1$ barycentric coordinates

\begin{align}
  \lambda_{\sigma(0)},\dots,\lambda_{\sigma(k)}.
\end{align}

 The barycentric monomials  $\b^r_{\vec{\alpha}}$ thus
 form a basis for the space $\ps{r}{\smp{f}}$.
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
import FEEC.Internal.Vector

import FEEC.Polynomial ( Polynomial, Term, term, terms, expandTerm,
                        evaluatePolynomial, derivePolynomial, integratePolynomial,
                        multiplyPolynomial,
                        barycentricCoordinates, barycentricGradient,
                        barycentricGradients, toPairs)
import qualified FEEC.Polynomial as P (degree, multiIndices, monomial,
                                       constant, polynomial)


import FEEC.Utility.Combinatorics(choose, factorial)
import FEEC.Utility.Print
--import FEEC.Utility.Utility(factorial)

\end{code}

%------------------------------------------------------------------------------%

\subsection{The \code{BernsteinPolynomial} Type}

The representation of Bernstein the \code{BernsteinPolynomial} type
uses the common polynomial type \code{Polynomial}. In addition to that
the Bernstein polynomial type has a field \code{simplex} which holds
the Simplex over which the polynomial is defined. In order to be able
to create constant polynomials independent of a simplex, the
\code{BernsteinPolynomial} type provides an additional constructor for
constant Bernstein polynomials.

%------------------------------------------------------------------------------%

\begin{code}

-- | Bernstein polynomial over a simplex. Represented by a normal polynomial
-- | internally and uses the generalized functions for evaluation and derivation.
data BernsteinPolynomial v r = Bernstein (Simplex v) (Polynomial r)
                             | Constant r
  deriving (Eq, Show)

-- pretty printing for Bernstein polyonmials
instance Pretty (BernsteinPolynomial v Double) where
    pPrint (Bernstein t p) = printBernstein ts
        where ts = map (expandTerm 0) (terms p)
    pPrint (Constant p) = text (show p)

-- | List multi-indices of the terms in the polynomial.
multiIndices :: (EuclideanSpace v, r ~ Scalar v)
             => BernsteinPolynomial v r -> [MI.MultiIndex]
multiIndices (Bernstein t p) = P.multiIndices n p
    where n = geometricalDimension t

degree :: BernsteinPolynomial v r -> Int
degree (Bernstein t p) = P.degree p
degree (Constant _) = 0

domain :: BernsteinPolynomial v r -> Simplex v
domain (Bernstein t _) = t
domain _ = error "Domain: No domain associated with constant Bernstein polynomial."
\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Constructors}

Since not all possible instances of the type \code{BernsteinPolynomial}
represent valid Bernstein polynomials, the constructors have to make sure that
the constructed polynomials are consistent. To be valid, all multi-indices must
have dimension $k+1$, where $k$ is the toppological dimension of the underlying
simplex $T = [\vec{v_0},\ldots,\vec{v_k}]$. Note that in Bernstein
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
polynomial :: (EuclideanSpace v, r ~ Scalar v)
           => Simplex v
           -> [(r, MI.MultiIndex)]
           -> BernsteinPolynomial v r
polynomial t l
    | (n1 == n2) && sameLength = Bernstein t (P.polynomial l)
    | otherwise = error "polynomial: Dimensions of Simplex and Polynomials do not match."
    where
      mis        = map (((-1)+) . dim . snd) l
      n1         = maximum mis
      n2         = topologicalDimension t
      sameLength = all (head mis ==) (tail mis)


-- | Create a Bernstein monomial over a given simplex from a given
-- | multi-index.
monomial :: (EuclideanSpace v, r ~ Scalar v)
         => Simplex v -> MI.MultiIndex -> BernsteinPolynomial v r
monomial t mi
    | n1 == n2 + 1 = Bernstein t (P.monomial mi)
    | otherwise   = error "monomial: Dimension of Simplex and Polynomials do not match."
  where n1 = dim mi
        n2 = topologicalDimension t

-- | Create a constant bernstein monomial.
constant :: (EuclideanSpace v, r ~ Scalar v) => r -> BernsteinPolynomial v r
constant = Constant

-- | Return a given barycentric coordinate in Bernstein represenation
barycentricCoordinate :: (EuclideanSpace v, r ~ Scalar v)
                      => Simplex v
                      -> Int
                      -> BernsteinPolynomial v r
barycentricCoordinate t i = monomial t (MI.unit (n+1) i)
    where n = topologicalDimension t

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Class Instantiations}

The Bernstein polynomials have the same algebraic structure as common
polynomials over $\R{n}$. That is they form a ring with respect to
addition and multiplication of polynomials and a vector space over
$\mathrm R$. This algebraic structure is implemented by the \code{Ring}
and \code{VectorSpace} class, respectively.

%------------------------------------------------------------------------------%

\begin{code}

-- | Bernstein polynomials as a vector space.
instance (EuclideanSpace v, r ~ Scalar v) => VectorSpace (BernsteinPolynomial v r) where
    type Scalar (BernsteinPolynomial v r) = r
    addV = addBernstein
    sclV = scaleBernstein

-- | Bernstein polynomials as a ring.
instance (EuclideanSpace v, r ~ Scalar v) => Ring (BernsteinPolynomial v r) where
    add = addBernstein
    addId = Constant addId
    addInv = scaleBernstein (sub addId mulId)

    mul = multiplyBernstein
    mulId = Constant mulId

    fromInt = Constant . fromInt

\end{code}

%------------------------------------------------------------------------------%

In addition to that, the Bernstein polynomials over a simplex $\smp{T}$ form an inner
 product space with $L^2$ inner product given by

\begin{align}
      \langle u, v \rangle = \int_\smp{T} u \cdot v \: d\vec{x}
\end{align}

This inner product over Bernstein polynomials is used to define the
inner product on the space of polynomial differential forms over the
simplex. In \code{FEEC}, the class \code{InnerProductSpace} is used to
represent inner product spaces.

%------------------------------------------------------------------------------%

\begin{code}
-- | Bernstein polynomials as inner product space.
instance (EuclideanSpace v, r ~ Scalar v) =>
    InnerProductSpace (BernsteinPolynomial v r) where
    inner  = innerBernstein
\end{code}

%------------------------------------------------------------------------------%

Finally, Bernstein polynomials are also functions that can be evaluated and
derived so they are declared an instance of the \code{Function}.

%------------------------------------------------------------------------------%

\begin{code}

instance (EuclideanSpace v, r ~ Scalar v) => Function (BernsteinPolynomial v r) v  where
  evaluate = evaluateBernstein
  derive   = deriveBernstein

\end{code}

%------------------------------------------------------------------------------%

\subsection{Arithmetic}

Addition, scaling and multiplication of Bernstein polynomials is
straight-forward and can be implemented using the functions
\code{addPolynomial}, \code{scalePolynomial} and \code{multiplyPolynomial}
provided by the \code{Polynomial} module.

%------------------------------------------------------------------------------%

\begin{code}

-- | Add Bernstein polynomials.
addBernstein :: (EuclideanSpace v, r ~ Scalar v)
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

-- | Multiply two Bernstein polynomials.
multiplyBernstein :: (EuclideanSpace v, r ~ Scalar v)
                  => BernsteinPolynomial v r
                  -> BernsteinPolynomial v r
                  -> BernsteinPolynomial v r
multiplyBernstein (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t1 = error "multiplyBernstein: Inconsistent simplices."
     | otherwise = Bernstein t1 (mul p1 p2)
multiplyBernstein (Constant c)      (Bernstein t1 p1) = Bernstein t1 (sclV c p1)
multiplyBernstein (Bernstein t1 p1) (Constant c)      = Bernstein t1 (sclV c p1)
multiplyBernstein (Constant c1)     (Constant c2)     = Constant (mul c1 c2)

\end{code}
%------------------------------------------------------------------------------%

\subsection{Evaluation}

Since a Bernstein polynomial is just a polynomial over the barycentric
coordinates of a simplex, evaluation of a Bernstein polynomial can be
implemented by first evaluating the barycentric coordinates at the given vector
and then evaluating the internal polynomial at the resulting vector.

%------------------------------------------------------------------------------%

\begin{code}
-- | Evaluate Bernstein polynomial by first evaluating the barycentric coordinates
-- | and then evaluating the internal polynomial at the resulting vector.
evaluateBernstein :: (EuclideanSpace v, r ~ Scalar v)
                     => v
                     -> BernsteinPolynomial v r
                     -> r
evaluateBernstein v (Bernstein t p) = evaluate vb p
    where vb = vector $ map (evaluate v) (barycentricCoordinates t)
evaluateBernstein _ (Constant c)    = c
\end{code}

%------------------------------------------------------------------------------%

\subsection{Derivation}

The derivative of a Bernstein monomial along a given direction in space is given
 by

$$
\begin{align}
  \frac{d}{dx_i}\B_{\vec{\alpha}}^{r}
    &= \sum_{j=0}^n \alpha_i \frac{d\lambda_j}{dx_i}\B_{\vec{\alpha^j}}^{r-1}
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
deriveMonomial :: (EuclideanSpace v, r ~ Scalar v)
               => Simplex v
               -> MI.MultiIndex
               -> [ Polynomial r ]
deriveMonomial t mi = [ sum' [sclV (grads j i) (dp j)  | j <- [0..n]] | i <- [0..n-1] ]
    where grads j i = toList (barycentricGradients t !! j) !! i
          dp j = if (mi' !! j) > 0
                 then P.polynomial [MI.derive j mi]
                 else P.constant addId
          sum' = foldl add addId
          mi'  = MI.toList mi :: [Int]
          n    = dim mi - 1

-- | Derive Bernstein polynomial.
deriveBernstein :: (EuclideanSpace v, r ~ Scalar v)
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
integratePolynomial :: (EuclideanSpace v, r ~ Scalar v)
                    => Simplex v                  -- t
                    -> BernsteinPolynomial v r    -- b
                    -> r
integratePolynomial t b = integrateOverSimplex q t (`evaluate` b)
    where q = div (r + 2) 2
          r = degree b

\end{code}

%------------------------------------------------------------------------------%

The second function computes the integral of a Bernstein polynomial over the
simplex it is defined over, which is given by
\begin{align}
\int_{\smp{T}}\B_{\vec{\alpha}}^{r} \: d\vec{x} &= \frac{\vec{\alpha}!}{r!}\frac{|\smp{T}|}%
{\left ( \begin{array}{c} k + d \\ k \end{array} \right )}
\end{align}

where $k$ is the topological dimension of the simplex. Since this requires the
simplex the polynomial is defined over to be known this function does not work
for the \code{Constant} constructor. The \code{redefine} function can be used to
add the information about the simplex to the Bernstein polynomial.

%------------------------------------------------------------------------------%

\begin{code}

-- | Closed-form integration of Bernstein polynomials over the simplex they are
-- | defined over.
integrateBernstein :: (EuclideanSpace v, r ~ Scalar v)
                      => BernsteinPolynomial v r
                      -> r
integrateBernstein b@(Bernstein t1 p) = sum' (map f (toPairs k p))
    where f (c, mi) = mul c (divide (mul (factorialMI mi) vol)
                                    (mul (factorial' (MI.degree mi)) (fac mi)))
          factorialMI = fromDouble . MI.factorial
          factorial' = fromDouble . factorial
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

-- | Inner product of Bernstein polynomials defined over a simplex T. If both
-- | polynomials are constant and have no associated simplex, a simplex with
-- | volume 1 is assumed.
innerBernstein :: (EuclideanSpace v, r ~ Scalar v)
                  => BernsteinPolynomial v r
                  -> BernsteinPolynomial v r
                  -> r
innerBernstein (Constant c1) (Constant c2) = mul c1 c2
innerBernstein b1 b2 = integrateBernstein (multiplyBernstein b1 b2)

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
proj :: (EuclideanSpace v, r ~ Scalar v)
     => Simplex v
     -> Int
     -> v
     -> r
proj t i    = dot u
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
extend :: (EuclideanSpace v, r ~ Scalar v)
       => Simplex v
       -> BernsteinPolynomial v r
       -> BernsteinPolynomial v r
extend t (Bernstein f p) = polynomial t (extend' (toPairs n' p))
    where extend' = map (\(c, mi) -> (c, MI.extend n (sigma f) mi))
          n = topologicalDimension t
          n' = topologicalDimension f
extend _ c = c

\end{code}
