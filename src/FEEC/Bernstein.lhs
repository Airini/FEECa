\section{Bernstein Polynomials}

A barycentric monomial $\vec{\lambda}^{\vec{\alpha}}$ is a product of the form

\begin{align}
  \vec{\lambda}^{\vec{\alpha}}(\vec{x}) &=
    \prod_{i = 0}^n \lambda_i^{\alpha_i}(\vec{x})
\end{align}

where the $\lambda_i$ are the barycentric coordinates with respect to a simplex
$T$. The barycentric polynomials of degree $r$ are obtained by scaling the
barycentric monomials with $|\vec{\alpha}| = r:

\begin{align}
  B_{\vec{\alpha}}^r &= \frac{r!}{\vec{\alpha}}
\end{aling}

%------------------------------------------------------------------------------%

\ignore{
\begin{code}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
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

\end{code}
}

%------------------------------------------------------------------------------%


%------------------------------------------------------------------------------%

\begin{code}

-- TODO: Enforce consistency of polynomial and simplex.

-- | Bernstein polynomial over a simplex. Represented by a normal polynomial
-- | internally and uses the generalized functions for evaluation and derivation.
<<<<<<< HEAD
data BernsteinPolynomial v r = Bernstein v (Polynomial r)
                           | Constant r
                             deriving (Eq, Show)

-- pretty printing for Bernstein polyonmials
instance Pretty (BernsteinPolynomial v r) where
    pPrint (Bernstein t p) = printPolynomial0 lambda (map (expandTerm 0) (terms p))
    pPrint (Constant p) = printPolynomial0 lambda (map (expandTerm 0) (terms (P.constant p)))

-- | List multi-indices of the terms in the polynomial.
multiIndices :: BernsteinPolynomial v r -> [MI.MultiIndex]
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
=======
data BernsteinPolynomial = Bernstein Simplex (Polynomial Double) |
                           Constant Double
--  deriving Show

-- pretty printing for Bernstein polyonmials
instance Show BernsteinPolynomial where
    show (Bernstein t p) = show $ printPolynomial0 lambda (map expandTerm (terms p))
    show (Constant p) = show $ printPolynomial0 lambda (map expandTerm (terms (P.constant p)))
>>>>>>> 0ca4c9a291099606d18e3227fef6fae1f3301f77


-- | Bernstein polynomials as a vector space.
<<<<<<< HEAD
instance Ring r => VectorSpace (BernsteinPolynomial v r) where
    type Scalar (BernsteinPolynomial v r) = r
    addV = addBernstein
    sclV = scaleBernstein

-- | Bernstein polynomials as a ring.
instance Ring r => Ring (BernsteinPolynomial v r) where
    add = addBernstein
    addId = Constant addId
    addInv = scaleBernstein (sub addId mulId)

    mul = multiplyBernstein
    mulId = Constant addId
=======
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
>>>>>>> 0ca4c9a291099606d18e3227fef6fae1f3301f77

    fromInt = Constant . fromInt

<<<<<<< HEAD
instance EuclideanSpace v r => Function (BernsteinPolynomial v r) v  where
  type Values (BernsteinPolynomial v r) v = r
--  type GeomUnit BernsteinPolynomial Vector = Simplex
  evaluate v (Bernstein t p) = evaluatePolynomial (evaluateMonomial lambda) p
      where lambda = vector (map (evaluate v) (barycentricCoordinates t))
  derive = deriveBernstein
  -- integrate t b@(Bernstein _ p) = integrateOverSimplex q t b
  --   where q = div (r + 2) 2
  --         r = degree p

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
polynomial :: EuclideanSpace v r
           => Simplex v
           -> [(r, MI.MultiIndex)]
           -> BernsteinPolynomial v r
polynomial t l
    | (n1 == n2 + 1) && sameLength = Bernstein t (P.polynomial l)
    | otherwise = error "polynomial: Dimensions of Simplex and Polynomials do not match."
    where
      mis        = map (dim . snd) l
      n1         = maximum mis
      n2         = topologicalDimension t
      sameLength = all ((head mis)==) (tail mis)
=======
instance Function BernsteinPolynomial Vector where
  type Values   BernsteinPolynomial Vector = Double
  type GeomUnit BernsteinPolynomial Vector = Simplex
  derive = deriveB
  integrate sx bp@(Constant a) = integralB $ Bernstein sx (sclV a (P.monomial (MI.zero (geometricalDimension sx))))
  integrate _sx bp@(Bernstein _sx' p) = integralB bp -- TODO: check equality for sx and sx'??
  evaluate = evalB
>>>>>>> 0ca4c9a291099606d18e3227fef6fae1f3301f77


-- | Create a Bernstein monomial over a given simplex from a given
-- | multi-index.
<<<<<<< HEAD
monomial :: EuclideanSpace v r
         => Simplex v -> MI.MultiIndex -> BernsteinPolynomial v r
monomial t mi
    | n1 == n2 + 1 = Bernstein t (P.monomial mi)
    | otherwise   = error "monomial: Dimension of Simplex and Polynomials do not match."
    where n1 = dim mi
          n2 = topologicalDimension t
=======
monomial :: Simplex -> MI.MultiIndex -> BernsteinPolynomial
monomial t mi = Bernstein t (P.monomial mi)
>>>>>>> 0ca4c9a291099606d18e3227fef6fae1f3301f77

-- | Create a constant bernstein monomial.
constant :: Field r => r -> BernsteinPolynomial r
constant = Constant

-- | Derivative of a Bernstein monomial
deriveMonomial :: Simplex -> Int -> MI.MultiIndex -> [Term Double]
deriveMonomial t d mi
    | d < dim mi = [Term (r i * dbs i) (MI.decrease d mi) | i <- [0..n]]
    | otherwise = error "deriveMonomial: Direction and multi-index have unequal lengths"
  where mi' = MI.toList mi
        r i = fromInteger (mi' !! i)
        bs = barycentricCoordinates t
        dbs i = evaluate (unitVector n d) (derive (unitVector n d) (bs !! i))
        n = geometricalDimension t

-- | Derive Bernstein polynomial.
deriveB :: Vector -> BernsteinPolynomial -> BernsteinPolynomial
deriveB v (Bernstein t p) = Bernstein t (derivePolynomial (deriveMonomial t) v p)
deriveB v (Constant c)    = Constant 0

-- | Add Bernstein polynomials.
<<<<<<< HEAD
addBernstein :: Ring r
             => BernsteinPolynomial r v
             -> BernsteinPolynomial r v
             -> BernsteinPolynomial r v
addBernstein (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t2 = error "addBernstein: Inconsistent simplices."
     | otherwise = Bernstein t1 (add p1 p2)
addBernstein (Constant c)    (Bernstein t p) = Bernstein t (add p (P.constant c))
addBernstein (Bernstein t p) (Constant c)    = Bernstein t (add p (P.constant c))
addBernstein (Constant c1)   (Constant c2)   = Constant (add c1 c2)

-- | Scale Bernstein polynomial.
scaleBernstein :: Ring r
               => r
               -> BernsteinPolynomial r v
               -> BernsteinPolynomial r v
scaleBernstein c  (Bernstein t p) = Bernstein t (sclV c p)
scaleBernstein c1 (Constant c2)   = Constant (mul c1 c2)

multiplyMonomial :: Field r
                    => MI.MultiIndex
                    -> MI.MultiIndex
                    -> Term r
multiplyMonomial mi1 mi2 = term (c, (MI.add mi1 mi2))
    where c = divide ((MI.add mi1 mi2) `MI.choose` mi1) ((r1 + r2) `choose` r1)
          r1 = (MI.degree mi1) :: Integer
          r2 = (MI.degree mi2) :: Integer

-- | Multiply two Bernstein polynomials.
multiplyBernstein :: Ring r
                  => BernsteinPolynomial v r
                  -> BernsteinPolynomial v r
                  -> BernsteinPolynomial v r
multiplyBernstein (Bernstein t1 p1) (Bernstein t2 p2)
     | t1 /= t1 = error "multiplyBernstein: Inconsistent simplices."
     | otherwise = Bernstein t1 (multiplyPolynomial multiplyMonomial p1 p2)
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
=======
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
    where lambda = map (evaluate v) (barycentricCoordinates t)
          n = geometricalDimension t
>>>>>>> 0ca4c9a291099606d18e3227fef6fae1f3301f77

-- | Evaluation of Bernstein polynomials.
evalB :: Vector -> BernsteinPolynomial -> Double
evalB v (Bernstein t p) = evaluatePolynomial (evalMonomial t) v p
evalB v (Constant c) = c

-- | List multi-indices of the terms in the polynomial.
multiIndices :: BernsteinPolynomial -> [MI.MultiIndex]
multiIndices (Bernstein t p) = P.multiIndices n p
    where n = geometricalDimension t

-- | Prefactor for Bernstein polynomials.
prefactor :: Int -> MI.MultiIndex -> Double
<<<<<<< HEAD
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
-- integrateBernstein :: Simplex -> BernsteinPolynomial -> Double
-- integrateBernstein t1 b@(Bernstein t2 p)
--     | t1 == t2  = sum (map f (toPairs k p))
--     | otherwise = integrate t1 b
--     where f (c, mi) = c * vol / ((k + MI.degree mi) `choose` k)
--           k = topologicalDimension t1
--           vol = volume t1
-- integrateBernstein t (Constant c) = c * (volume t)

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
=======
prefactor n a = fromInteger (factorial n) / fromInteger (MI.factorial a)
>>>>>>> 0ca4c9a291099606d18e3227fef6fae1f3301f77

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
integrateBernstein (Bernstein t p)  = sum (map f (undefined {-XXX: check :Sdegrees p-}))
    where f mi = vol / (fromIntegral ((fromIntegral n + MI.degree mi) `choose` fromIntegral n))
          vol = volume t
          n = geometricalDimension t -- check (cf: below)

-- | Integrate Bernstein polynomial over the simplex it is defined over.
integralB :: BernsteinPolynomial -> Double
integralB (Constant _) = error "integral: Cannot integrate Bernstein polynomial without associated simplex."
integralB (Bernstein t p) = integrate {-n f-} t p
    where n = geometricalDimension t
          f mi = volume t / fromIntegral ((n + MI.degree mi) `choose` MI.degree mi)

-- | Extend a Bernstein polynomial defined on a subsimplex f to the simplex t.
extend :: Simplex -> BernsteinPolynomial -> BernsteinPolynomial
extend t (Bernstein f p) = Bernstein t (polynomial (extend' (undefined n' p))) -- XXX: toPairs!! gone missing
    where extend' = map (\(c, mi) -> (c, MI.extend n (sigma f) mi))
          n = topologicalDimension t
          n' = topologicalDimension f
extend _ c = c

\end{code}