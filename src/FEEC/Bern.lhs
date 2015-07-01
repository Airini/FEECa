\section{Bernstein Polynomials}

A barycentric monomial of degree $r$ $B^r_{\vec{\alpha}}$ defined over a simplex
$T$ is a product of the form

\begin{align}
  \B_{\vec{\alpha}}^r &=
    \frac{r!}{\vec{\alpha}1}\prod_{i = 0}^n \lambda_i^{\alpha_i}(\vec{x})
\end{align}

where the $\lambda_i$ are the barycentric coordinates on $T$. For the definition
of factorials with multi-indices as arguments see section \ref{sec:MI_mathops}.

The barycentric polynomials of degree $r$ over a given simplex form a basis of
polynomials of degree $r$ over $\R{n}$.

%------------------------------------------------------------------------------%

\ignore{
\begin{code}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FEEC.Bern where

import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Internal.Simplex
import FEEC.Internal.Spaces hiding (pow)
import FEEC.Internal.Vector

import FEEC.Polynomial (Polynomial(..), Term, term, terms, expandTerm,
                        evaluatePolynomial, derivePolynomial, -- multiplyPolynomial,
                        barycentricCoordinates, barycentricGradient,
                        barycentricGradients) -- , toPairs)
import qualified FEEC.Polynomial as P (multiIndices, monomial, constant, polynomial)


import FEEC.Utility.Combinatorics(choose, factorial)
import FEEC.Utility.Print
import FEEC.Utility.Utility

\end{code}
}

%------------------------------------------------------------------------------%

\subsection{The \code{BernsteinPolynomial} Type}

Since Bernstein polynomials can be represented in the same way as common
polynomials a general Bernstein polynomial is represented by a simplex and a
polynomial. In order to be able to create constant polynomials independent of a
simplex, the \code{BernsteinPolynomial} type provides an additional constructor
for constant Bernstein polynomials.

%------------------------------------------------------------------------------%

\begin{code}

-- TODO: Enforce consistency of polynomial and simplex.

-- | Bernstein polynomial over a simplex. Represented by a normal polynomial
-- | internally and uses the generalized functions for evaluation and derivation.
data BernsteinPolynomial = Bernstein Simplex (Polynomial Double)
                         | Constant Double
                           deriving (Eq, Show)

-- pretty printing for Bernstein polyonmials
instance Pretty BernsteinPolynomial where
    pPrint (Bernstein t p) = printPolynomial0 lambda (map (expandTerm 0) (terms p))
    pPrint (Constant p) = printPolynomial0 lambda (map (expandTerm 0) (terms (P.constant p)))

-- | List multi-indices of the terms in the polynomial.
multiIndices :: BernsteinPolynomial -> [MI.MultiIndex]
multiIndices (Bernstein t p) = P.multiIndices n p
    where n = geometricalDimension t

\end{code}

%------------------------------------------------------------------------------%
\subsubsection{Algebraic Structure}

Since the Bernstein polynomials are just one special representation of the
polynomials over $\R{n}$, they have the same algebraic structure. That is they
form a ring with respect to addition and multiplication of polynomials and a
vector space over $\mathrm R$. The algebraic structure is implemented by the
\code{Ring} and \code{VectorSpace} class, respectively.

Moreover, Bernstein polynomials are also functions that can be evaluated,
derived and itegrated so they are declared an instance of the \code{Function}
class together with the \code{Vector} type.

%------------------------------------------------------------------------------%

\begin{code}

-- | Bernstein polynomials as a vector space.
instance VectorSpace BernsteinPolynomial where
    type Scalar BernsteinPolynomial = Double
    addV = addBernstein
    sclV = scaleBernstein

-- | Bernstein polynomials as a ring.
instance Ring BernsteinPolynomial where
    add = addBernstein
    addId = Constant 0.0
    addInv = scaleBernstein (-1)

    mul = multiplyBernstein
    mulId = Constant 1.0

    fromInt = Constant . fromIntegral

instance Function BernsteinPolynomial Vector where
  type Values   BernsteinPolynomial Vector = Double
  type GeomUnit BernsteinPolynomial Vector = Simplex
  evaluate v (Bernstein t p) = evaluatePolynomial (evaluateMonomial lambda) p
      where lambda = vector (map (evaluate v) (barycentricCoordinates t))
  derive = deriveBernstein
  integrate t b@(Bernstein _ p) = integrateOverSimplex q t b
    where q = div (r + 2) 2
          r = degree p

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Constructors}

Since not all possible instances of the type \code{BernsteinPolynomial}
represent valid Bernstein polynomials, the constructors have to make sure that
the constructed polynomials are consistent. To be valid, all multi-indices must
have the same dimension which is just the topological dimension of the simplex
$T$ plus one. Note that in Bernstein representation the multi-indices
representing the polynomial are of dimension $n+1$, while in the monomial
representation they have length $n$.

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
polynomial :: Simplex -> [(Double, MI.MultiIndex)] -> BernsteinPolynomial
polynomial t l
    | (n1 == n2 + 1) && sameLength = Bernstein t (P.polynomial l)
    | otherwise = error "polynomial: Dimensions of Simplex and Polynomials do not match."
    where
      mis        = map (dim . snd) l
      n1         = maximum mis
      n2         = topologicalDimension t
      sameLength = all ((head mis)==) (tail mis)


-- | Create a Bernstein monomial over a given simplex from a given
-- | multi-index.
monomial :: Simplex -> MI.MultiIndex -> BernsteinPolynomial
monomial t mi
    | n1 == n2 + 1 = Bernstein t (P.monomial mi)
    | otherwise   = error "monomial: Dimension of Simplex and Polynomials do not match."
    where n1 = dim mi
          n2 = topologicalDimension t

-- | Create a constant bernstein monomial.
constant :: Double -> BernsteinPolynomial
constant = Constant

\end{code}

%------------------------------------------------------------------------------%

\subsection{Arithmetic}

Addition and scaling of Bernstein polynomial is straight-forward and can be
implemented using the arithmetic on polynomials.

For the multiplication of Bernstein polynomials one need to take into account
the prefactors of the two polynomials:

\begin{align}
  \B{\vec{\alpha}_1}{r_1}\cdot\B{\vec{\alpha}_2}{r_2} &=
    \frac{{\vec{\alpha}_1 + \vec{alpha}_2} \choose \vec{\alpha}_1}
         {{r_1 + r_2} \choose r_1}
    \B{\vec{\alpha_1}+\vec{\alpha}_2}{r_1+r_2}
\end{align}

Here we can use the general implementation of multiplication of polynomials
\code{multiplyPolynomials} provided by the \code{Polynomial} module.

%------------------------------------------------------------------------------%

\begin{code}

-- | Add Bernstein polynomials.
addBernstein :: BernsteinPolynomial -> BernsteinPolynomial -> BernsteinPolynomial
addBernstein (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t2 = error "addBernstein: Inconsistent simplices."
     | otherwise = Bernstein t1 (add p1 p2)
addBernstein (Constant c)    (Bernstein t p) = Bernstein t (add p (P.constant c))
addBernstein (Bernstein t p) (Constant c)    = Bernstein t (add p (P.constant c))
addBernstein (Constant c1)   (Constant c2)   = Constant (c1 + c2)

-- | Scale Bernstein polynomial.
scaleBernstein :: Double -> BernsteinPolynomial -> BernsteinPolynomial
scaleBernstein c  (Bernstein t p) = Bernstein t (sclV c p)
scaleBernstein c1 (Constant c2)   = Constant (c1 * c2)

multiplyMonomial :: (Ring a, Fractional a) => MI.MultiIndex -> MI.MultiIndex -> Term a
multiplyMonomial mi1 mi2 = term (c, (MI.add mi1 mi2))
    where c = ((MI.add mi1 mi2) `MI.choose` mi1) / ((r1 + r2) `choose` r1)
          r1 = (MI.degree mi1) :: Integer
          r2 = (MI.degree mi2) :: Integer

-- | Multiply two Bernstein polynomials.
multiplyBernstein :: BernsteinPolynomial -> BernsteinPolynomial -> BernsteinPolynomial
multiplyBernstein (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t1 = error "multiplyBernstein: Inconsistent simplices."
     | otherwise = Bernstein t1 (undefined multiplyMonomial p1 p2)
multiplyBernstein (Constant c)      (Bernstein t1 p1) = Bernstein t1 (sclV c p1)
multiplyBernstein (Bernstein t1 p1) (Constant c)      = Bernstein t1 (sclV c p1)
multiplyBernstein (Constant c1)     (Constant c2)     = Constant (c1 * c2)

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
-- TODO: change vector to point
evaluateMonomial :: Vector -> MI.MultiIndex -> Double
evaluateMonomial lambda mi = prefactor r mi * pow lambda mi
    where r = MI.degree mi

-- | Prefactor for Bernstein polynomials.
prefactor :: Int -> MI.MultiIndex -> Double
prefactor r a = fromInteger (factorial r) / fromInteger (MI.factorial a)

\end{code}

%------------------------------------------------------------------------------%

\subsection{Derivaiton}

The derivative of a Bernstein monomial along a given space dimension is given by

\begin{align}
  \frac{d}{dx_i}\B{\vec{\alpha}}{r}
    &= r\sum_{j=0}^n \frac{d\lambda_j}{dx_i}\R{\vec{\alpha^j}}{r-1}
\end{align}
where $\vec{\alpha^j}$ is the multi-index $\vec{\alpha}$ with the exponent at
position $j$ decreased by one and $\frac{d\lambda_j}{dx_i}$ is the $i$th
component of the gradient of the $j$th barycentric coordinate. Note that the
factor $\alpha_i$ in the derivation is absorbed into the prefactor
$\frac{r!}{\vec{\alpha}}$.

The derivation of Bernstein polynomials is again implemented using the general
function provided by the \module{Polynomial} module. All that has to be done is
therefore to implement the derivation of Bernstein monomials.

%------------------------------------------------------------------------------%

\begin{code}

-- | Derivative of a Bernstein monomial
deriveMonomial :: Simplex -> Int -> MI.MultiIndex -> [Term Double]
deriveMonomial t d mi
    | d < dim mi = [term ((r * (dbs !! i) !! d), (MI.decrease d mi))  | i <- [0..n]]
    | otherwise = error "deriveMonomial: Direction and multi-index have unequal lengths"
  where dbs = barycentricGradients t
        r   = fromIntegral (dim mi)
        n   = topologicalDimension t

-- | Derive Bernstein polynomial.
deriveBernstein :: Vector -> BernsteinPolynomial -> BernsteinPolynomial
deriveBernstein v (Bernstein t p) = Bernstein t (derivePolynomial (deriveMonomial t) v p)
deriveBernstein v (Constant c)    = Constant 0

\end{code}

%------------------------------------------------------------------------------%

\subsection{Integration}

For integration of Bernstein polynomials we provide two functions: The first one
implements the \code{Function} interface for the integration over an arbitrary
simplex using the general implementation provided by the
\code{integratOverSimplex} function from the \module{Simplex} module. The second
one implements integration over the simplex $T$ over which the Bernstein
polynomial is defined.

\begin{align}
\int_T\B{\vec{\alpha}}{r} \: d\vec{x} &= \frac{|T|}{{k + d} \choose {k}}
\end{align}

where $k$ is the topological dimension of the simplex.

%------------------------------------------------------------------------------%

\begin{code}

-- | Closed-form integration of Bernstein polynomials over the simplex they are
-- | defined over. Falls back to standard integration if the provided simplex
-- | does not equal the simplex the bernstein polynomial is defined over. 
integrateBernstein :: Simplex -> BernsteinPolynomial -> Double
integrateBernstein t1 b@(Bernstein t2 p)
    | t1 == t2  = sum (map f (undefined k p))
    | otherwise = integrate t1 b
    where f (c, mi) = c * vol / ((k + MI.degree mi) `choose` k)
          k = topologicalDimension t1
          vol = volume t1
integrateBernstein t (Constant c) = c * (volume t)

-- | Redefined Bernstein polynomial over a different simplex or define simplex
-- | for constant bernstein polynomial.
redefine :: Simplex -> BernsteinPolynomial -> BernsteinPolynomial
redefine t1 (Bernstein t2 p) = Bernstein t1 p
redefine t (Constant c)      = Bernstein t (P.constant c)

\end{code}

%------------------------------------------------------------------------------%

\subsection{Basis for Differential Forms}

The gradients of the barycentric coordinates

\begin{align}
  d\lambda_0,\ldots,d\lambda_n
\end{align}

of a simplex $T$ span the space of alternating one-forms in $\R{n}$. The
projection along the gradient then amounts to simply forming the dot product
with the gradient vector.

%------------------------------------------------------------------------------%

\begin{code}

-- | Projection fuction for gradients of barycentric coordinates as basis for
-- | the space of alternating forms.
proj :: Simplex -> Int -> Vector -> Double
proj t i v = sum (zipWith (*) grad (toList v))
    where grad = barycentricGradient t i

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
extend :: Simplex -> BernsteinPolynomial -> BernsteinPolynomial
extend t (Bernstein f p) = polynomial t (extend' (undefined n' p))
    where extend' = map (\(c, mi) -> (c, MI.extend n (sigma f) mi))
          n = topologicalDimension t
          n' = topologicalDimension f
extend _ c = c

\end{code}
