\section{Forms}

In what follows, the term \textit{forms} will be used to denote multi-linear, alternating
maps from the $k$-product of a vector space $V$ with itself into a vector space $W$. Thus,
a $k$-form $\omega$ is a mapping
\begin{align}
 \underbrace{V \times \ldots \times V}_{k\text{ times}} \rightarrow W
\end{align}
satisfying the following properties:
\begin{itemize}
  \item Linearity:
\begin{align}
\omega \left(\vec{v}_1,\ldots,\alpha \vec{v}_i + \beta\vec{w},\ldots,\vec{v}_k\right)
&=  \alpha \omega\left(\vec{v}_1,\ldots,\vec{v}_i,\ldots,\vec{v}_k\right)
    + \beta \omega\left(\vec{v}_1,\ldots,\vec{w},\ldots,\vec{v}_k\right)
\end{align}
\item Alternating:
\begin{align}
\omega\left(\vec{v}_1,\ldots,\vec{v}_i,\ldots,\vec{v}_j,\ldots,\vec{v}_k\right)
&= - \omega \left (\vec{v}_1,\ldots,\vec{v}_j,\ldots,\vec{v}_i,\ldots,\vec{v}_k \right)
\end{align}
\end{itemize}

The space of $k$-arity forms over $V$ mapping into $W$ is denoted by $\Lambda^k(V; W)$.
A general implementation of alternating forms can be obtained by representing a form
$\omega: V \times \ldots \times V \rightarrow W$ as a an element $\vec{w}$ of the vector
space $W$ scaled by an alternating form mapping into the scalars of the vector space $W$:
\begin{align}
\omega = \vec{w} \hat{\omega}
\end{align}
Here only vector spaces over the reals will be
considered so that the form $\hat{\vec{w}}$ can be assumed to be an alternating form
mapping into $\mathbb{R}$. Hence the implementation of forms mapping into an arbitary
vector space $W$ can be implemented almost completely independent of the space $W$ and
only requires the ability to scale the element $\vec{w}$ with an arbitrary scalar
$c \in \mathbb{R}$.

%------------------------------------------------------------------------------%

\begin{code}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module FEECa.Internal.Form (
  -- * Generic form types
  Form (Form, arity, dimension, terms), split

  -- * Predefined primitive constructors
  , zeroForm, oneForm

  -- * Form operations
  , apply

  -- * Form
  , LinearCombination(..), interiorProduct, innerProduct

  -- * Simplification
  , combine


  ) where


import Control.Applicative
import Data.List (intersect, sort, foldl')
import qualified Data.Matrix as M
import FEECa.Internal.Spaces hiding( inner )
import qualified FEECa.Internal.Spaces as S( inner )
import FEECa.Utility.Discrete
import FEECa.Utility.Utility (pairM, sumV, expSign, sign)
import FEECa.Utility.Print (Pretty(..), printForm)
import FEECa.Utility.Combinatorics
\end{code}
%------------------------------------------------------------------------------%

\subsection{Representation}

For the representation of forms the existence of a set of coordinate functions
\begin{align}
       \lambda_i: V \to \mathbb{R} \text{ for } i = 1,\ldots,n
\end{align}
is assumed. Moreover, the directional derivatives of these coordinate functions are
assumed to span the space of one-forms over $V$ by taking
\begin{align}
d\lambda_i(\vec{v}) = \frac{d\lambda_i(x\vec{v})}{dx}
\end{align}

General one-forms are then represented as linear combinations over
this spanning set. The basis elements of spaces of higher-arity forms are obtained from
the wedge products of the 1-form basis elements. In the code, higher-arity basis
forms are represented by lists of integers \code{[Int]} containing the indices of
basis elements in the wedge product.

The \code{Form} data type represents a general $k$-form over an n-dimensional Euclidean
vector space. Note that we are dealing here with two different vector spaces: The vector
space $V$ over which the form is defined, i.e. whose elements are the arguments to which
the form is applied, and the vector space $W$ into which the form maps. The vector space
$W$ is represented by the type parameter \code{w} of the \code{Form} data type.

%------------------------------------------------------------------------------%
\begin{code}
-- | Type alias for the representation of linear combinations.
type LinearCombination c is = [(c, is)]

-- | A general alternating k-form defined over n-dimensional Euclidean space.
data Form w =
    Form  { arity     :: Int
          , dimension :: Int
          , terms     :: LinearCombination w [Int]}
  deriving (Eq, Show)
\end{code}
%------------------------------------------------------------------------------%
\subsubsection{Constructors}
To construct primitive forms three constructor functions are provided:

 The \code{zeroForm} function creates a zero-form from a given element $\vec{w} \in W$.
The \code{oneForm} function creates a one form
\begin{align}
           \omega(\vec{v}) = \frac{d\lambda_i}{d\vec{v}}\vec{w}
\end{align}
for a given index $i$ and element $\vec{w} \in W$:

%------------------------------------------------------------------------------%
\begin{code}
zeroForm :: VectorSpace w => Int -> w -> Form w
zeroForm n w = Form 0 n [(w, [])]

oneForm :: VectorSpace w => Int -> w -> Int -> Form w
oneForm n w i = Form 1 n [(w, [i])]
\end{code}
%------------------------------------------------------------------------------%
\subsection{Evaluation}

As mentioned above, general forms $\omega \in \Lambda^k(V;W)$ can be
implemented as products of a vector $\vec{w} \in W$ and a form
$\hat{\omega} \in \Lambda^k(V,\mathbb{R})$. The value of the application
of a set of vectors $\vec{v}_1,\ldots,\vec{v}_k$ to the basis form
$dx_{i_1} \wedge \ldots \wedge dx_{i_k}$ can
be computed as the determinant of the matrix $(\mathbf{A})_{i,j} = d\lambda_i(\vec{v}_j)$:

\begin{align}
  dx_{i_1} \wedge \ldots \wedge dx_{i_k}(\vec{v}_1,\ldots,\vec{v}_k) &=
  \text{det}(d\lambda_i(\vec{v}_j))
\end{align}

The FEECa implementation uses the \code{matrix} package for the computation of
the determinant. Since this package requires the matrix elements to implement
the \code{Ord} and \code{Fractional} type classes, the \code{Numeric} type class
has been introduce to combine the properties required for numerical
computations.

%------------------------------------------------------------------------------%
\begin{code}
-- | Application to a single basis form.
applyLC' :: (Numeric f, VectorSpace w, f ~ Scalar w)
       => (Int -> v -> f)
       -> [v]
       -> (w, [Int])
       -> w
applyLC' d vs (w, is) = sclV (M.detLU $ M.fromList k k $ pure d <*> is <*> vs) w
  where k = length vs

-- | Extension to linear combinations.
applyLC :: (Numeric f, VectorSpace w, f ~ Scalar w)
      => (Int -> v -> f)
      -> LinearCombination w [Int]
      -> [v]
      -> w
applyLC d ls [] = sumV (map fst ls)
applyLC d ls vs = sumV (map (applyLC' d vs) ls)

-- | Application to forms.
apply :: (Numeric f, VectorSpace w, f ~ Scalar w)
      => (Int -> v -> f)
      -> Form w
      -> [v]
      -> w
apply d (Form _ _ lc) vs = applyLC d lc vs

\end{code}
%------------------------------------------------------------------------------%
\subsection{Arithmetic}

Forms mapping into a vector space $\vec{W}$ form themself a vector space over
the scalars of the space $\vec{W}$. This structure is made accessible in the
code by implementing the \code{VectorSpace} type class. Addition of forms is
implemented by simply concatenating the lists representing the linear combinations.
Scaling is implemented by scaling all coefficients in the linear combination.


%------------------------------------------------------------------------------%
\begin{code}
addLC :: LinearCombination s v -> LinearCombination s v -> LinearCombination s v
addLC l1 l2 = l1 ++ l2

sclLC :: (VectorSpace v, s ~ Scalar v) => s -> LinearCombination v i -> LinearCombination v i
sclLC c l = fmap (sclV' c) l
  where sclV' c (a, is) = (sclV c a, is)

-- | Forms over a vector space form themself a vector space.
instance VectorSpace v => VectorSpace (Form v) where
  type Scalar (Form v) = Scalar v
  addV (Form j m lc1) (Form k n lc2) = Form k n (addLC lc1 lc2)
  sclV c (Form k n lc2) = Form k n (sclLC c lc2)
\end{code}
%------------------------------------------------------------------------------%
\subsubsection{Wedge Product}
The wedge product of two forms $\omega, \eta$ is defined as

\begin{align}
  \eta \wedge \omega (\vec{v}_1,\ldots,\vec{v}_{j+k}) = \sum_{\sigma} \quad \text{sign}(\sigma)
  \omega(v_{\sigma(1)},\ldots,v_{\sigma(j)})\eta(v_{\sigma(j + 1), \ldots, \sigma(j + k)})
\end{align}

where the sum is over all permutations $\sigma$ of the vectors
$\vec{v}_1,\ldots,\vec{v}_{j+k}$. The wedge product of two basis
elements of the spaces $dx_{i_1} \wedge\ldots \wedge dx_{i_j} \in
\Lambda^j(V;W)$ and $dx_{i_{j+1}} \wedge\ldots \wedge dx_{i_{k+j}} \in
\Lambda^k(V;W)$ is given by the basis element

\begin{align}
dx_{i_{1}} \wedge\ldots \wedge dx_{i_{k+j}} \in \Lambda^{j + k}(V;W)
\end{align}

The wedge product of the general forms can then be obtained by
extending the above definition by linearity of linear combinations of
basis forms. The wedge product of two linear combinations of basis
forms can thus be computed by concatenating the index lists
representing the basis elements in the linear combination and
multiplying the corresponding coefficients for all mutual pairs of
terms from the two linear combinations.

%------------------------------------------------------------------------------%
\begin{code}
-- | Symbolic computaiton of the wedge product of two forms
-- | represented by linear combinations of basis elements.
wedgeLC :: Ring s
       => LinearCombination s [Int]
       -> LinearCombination s [Int]
       -> LinearCombination s [Int]
wedgeLC l1 l2 = pure mul' <*> l1 <*> l2
  where mul' (a1, is1) (a2, is2) = (mul a1 a2, is1 ++ is2)

-- | The wedge product of two forms.
(/\) :: Ring s => Form s -> Form s -> Form s
(/\) (Form j m lc1) (Form k n lc2) = Form (j + k) n (wedgeLC lc1 lc2)
\end{code}
%------------------------------------------------------------------------------%

\subsubsection{Interior Product}

The interior product maps a  $k$-form $\omega \in \Lambda^k(V; W)$ and a vector $\vec{v} \in V$
into $\Lambda^{k-1}(V; W)$ via
\begin{align}
    \omega \lrcorner \vec{v}_1 (\vec{v}_2,\ldots,\vec{v}_k)
        &= \omega(\vec{v}_1,\vec{v}_2,\ldots, \vec{v}_k)
\end{align}

Given a form $\omega \in \Lambda^k(V,W)$ represented by a vector $\vec{w}$ and a form
$\hat{\omega} \in \Lambda^k(V, \mathbb{R})$ the result of the application of the vectors
$v_1,\ldots,v_k$ to $\omega$ is given by
\begin{align}
  \omega(v_1,\ldots,v_k) &= \vec{w} \: \text{det}(\lambda_i(v_j))
\end{align}
  where $\text{det}(\lambda_i(v_j))$ is the determinant of the matrix
$(\mathbf{A})_{i,j} = \lambda_i(v_j)$. The interior product can thus be computed
by applying Laplace's formula to the above determinant. This gives:

\begin{align}
dx_1 \wedge \ldots \wedge dx_k \lrcorner \vec{v} =
  \sum_{i = 1,\ldots,k} (-1)^{1 + i} \lambda_i(\vec{v})
  dx_1 \wedge \ldots \wedge dx_{i-1} \wedge dx_{i+1} \wedge \ldots \wedge dx_k
\end{align}

Again this can be extended to linear combinations of basis forms by the linearity
of the interior product. For the computation of the interior product the
coordinate functions $\lambda_i: V \to \mathbb{R}$ are required. Here, they are
given by a function argument of type \code{Int -> v -> f} and the type \code{f}
is required to be the scalar type associated to the vector type \code{v}.

%------------------------------------------------------------------------------%
\begin{code}
-- | Interior product of a basis form and a vector.
interiorProductLC' :: (Ring f)
                 => (Int -> v -> f) -- The projection map
                 -> [Int]           -- The wedge product indices
                 -> v               -- The vector to apply
                 -> [(f, [Int])]
interiorProductLC' d l v = [(mul s c, i) | (s, c, i) <- zip3 signs cols indices]
  where cols    = pure d <*> l <*> pure v
        signs   = alternatingSequence
        indices = sublists l

-- | Extension of the exterior product on basis forms to linear combination of basis
-- | forms.
interiorProductLC :: (VectorSpace v, VectorSpace w, f ~ Scalar w)
                => (Int -> v -> f)            -- The projection map
                -> LinearCombination w [Int]
                -> v
                -> LinearCombination w [Int]
interiorProductLC dx l v  = concatMap applyInterior l
  where applyInterior (f, is) = map (scale f) (interiorProductLC' dx is v)
        scale c  (a, b)       = (sclV a c, b)

-- | Interior product of forms.
interiorProduct :: (VectorSpace v, VectorSpace w, f ~ Scalar w)
                => (Int -> v -> f)            -- The projection map
                -> Form w
                -> v
                -> Form w
interiorProduct dx (Form k n lc) v = Form (k - 1) n (interiorProductLC dx lc v)

alternatingSequence :: Ring f => [f]
alternatingSequence = mulId : map addInv alternatingSequence
\end{code}
%------------------------------------------------------------------------------%

\subsection{Inner Product}

If an inner product is defined on the space $W$, an inner product can be defined
on the space of forms $\Lambda^k(V;W)$ using

\begin{align}
  \langle \omega, \eta \rangle & =
\sum_\sigma \eta(\vec{v}_{\sigma(1)},\ldots,\vec{v}_{\sigma(k)}) \cdot
\eta(\vec{v}_{\sigma(1)},\ldots,\vec{v}_{\sigma(k)})
\end{align}

where the sum is over increasing k-sequences $\sigma: \{1,\ldots,k\} \to \{1,\ldots,n\}$
and $\cdot$ is used to denote the inner product on $W$.

%------------------------------------------------------------------------------%
\begin{code}
innerProduct :: (EuclideanSpace v, InnerProductSpace w, Scalar w ~ r, Numeric r)
             => (Int -> v -> r) -- The projection map
             -> Form w
             -> Form w
             -> r
innerProduct d omega eta = sum' [S.inner (omega' vs) (eta' vs) | vs <- kSublists k bs]
  where k      = arity omega
        n      = dimension omega
        omega' = apply d omega
        eta'   = apply d eta
        bs     = orthonormalBasis (dimension omega)
        sum'   = foldl' add addId
\end{code}
%------------------------------------------------------------------------------%

\subsection{Simplification}
While not strictly necessary for the correctness of the code, simplifying forms
will greatly improve the readability of the output and might also yield
performance benefits. Note that the reprsentation of basis form by indices
$i_1, \ldots, i_k$ is ambiguous in the sense that basis elements whose indices
are related by a permutation a linearly dependent. To resolve this ambiguity
we define a form to be in normal form when the indices $i_1,\ldots, i_k$ are
in ascending order. Operations such as the interior product or the wedge
product of forms may yield basis forms that are not in normal form. To properly
simplify these forms the \code{normalForm} method transforms a linear combination
of basis forms into normal form by computing the number of inversions of the index list
(c.f. \cite{wikiInv}) and scaling the basis element accordingly.

For the simplification of forms, terms involving identical basis elements are
combined by adding there coefficients. This of course requires the coefficients
to provide a ring structure.

%------------------------------------------------------------------------------%
\begin{code}
-- | Convert basis element to normal form.
normalForm' :: Ring f => (f, [Int]) -> (f, [Int])
normalForm' (c, is) = if ((parity is) == 0) then (c, sort is) else (addInv c, sort is)

-- | Convert complete linear combination to normal form.
normalForm :: Ring f => LinearCombination f [Int] -> LinearCombination f [Int]
normalForm cs = map normalForm' cs

-- | Combine terms in the linear combination with identical index lists by adding
-- | their coefficients.
combineLC' :: Ring f => [(f, [Int])] -> [(f, [Int])]
combineLC' []             = []
combineLC' ((f, is) : ls) = (foldl add f (map fst ls'), is) : combineLC' ls''
  where ls'  = filter ((is ==) . snd) ls
        ls'' = filter ((is /=) . snd) ls

-- | Convert terms in linear combination to normal form and combine them.
combineLC :: Ring f => LinearCombination f [Int] -> LinearCombination f [Int]
combineLC cs = combineLC' $ map normalForm' cs

-- | Combine terms in the linear combination representing the form.
combine :: Ring f => Form f -> Form f
combine (Form k n lc) = Form k n $ combineLC $ normalForm lc

\end{code}
%------------------------------------------------------------------------------%


%------------------------------------------------------------------------------%
\begin{code}


split :: (Ring v, VectorSpace w, Scalar w ~ v)
      => Form w -> ([w], [Form v])
split (Form k n cs)   = unzip $ map split' cs
  where  split' (a,b) = (a, Form k n [(mulId, b)])

-- terms [(17, [1,2]), (38, [1,3])] = 17*dx1/\dx2 + 38*dx1/\dx3

--- NB: because of ((,) t) 's functorial nature, maybe it could make sense to
---   rearrange our terms so as to have them be (inds,coeff) like they used to be?
--- XXX: change so as to inspect result (in case f zeroes out a coeff?)
instance Functor Form where
  fmap f (Form k n cs) = Form k n (map (pairM f id) cs)


instance Pretty f => Pretty (Form f) where
  pPrint (Form k n cs) = printForm "dx" "0" pPrint cs -- show or pPrint...
  {- show k ++ "-form in " ++ show n ++ " dimensions: " ++
                          show (printForm "dx" "0" show cs)-}


-- NB: will be (i -> i -> Ordering) once we normalise all products to be in
--      their normal form - an increasing list
combineWithBy :: (a -> a -> a) -> (i -> i -> Bool)
              -> [(a, i)] -> [(a, i)] -> [(a,i)]
combineWithBy f p es1 es2 = foldl ins es1 es2
  where ins [] x = [x]
        ins (y:ys) x | p (snd x) (snd y) = (fst x `f` fst y, snd y) : ys
                     | otherwise         = y : ins ys x

-- XXX: have combinator
--        aggregate f p cs1 cs2
--      where f :: a -> a -> a (or b -> c)
--            p :: MultiIndex -> MultiIndex -> Bool
--            cs1, cs2 :: [(a, MultiIndex)]
--      such that combines elements from cs2 with those in cs1 (sequentially,
--      no repeat, or filtering those appropriate) with p defining the "quotient"
--      class, f defining the transformation on coeff's
--      OPT: also a MultiIndex transformation? or a separte combinator for it?

-- | Sum of forms
-- Shall we do: permutation simplification/identification
(+++) :: Ring f => Form f -> Form f -> Form f
omega +++ eta
    | degNEq omega eta = errForm "(+++)" BiDegEq
    | spaNEq omega eta = errForm "(+++)" BiSpaEq
    | otherwise = Form (arity eta) (dimension eta)
                       (step (terms omega) (terms eta))
  where step [] ys = ys
        step xs [] = xs
        step (x:xs) (y:ys)
          | snd x == snd y = let z = add (fst x) (fst y) in
              if fst x /= (addInv . fst) y then (z, snd x) : step xs ys
                            else step xs ys
          | snd x < snd y  = x : step xs (y:ys)
          | otherwise      = y : step (x:xs) ys


-- | (Exterior) Product of forms
(//\\) :: Ring f => Form f -> Form f -> Form f
omega //\\ eta
    | spaNEq omega eta = errForm "(//\\\\)" BiSpaEq
    | otherwise = Form (arity omega + arity eta) (dimension eta)
                       (concatMap (\d -> map (`combine` d) (dxs d)) (terms eta))
  where dxs     (_,ys) = filter (null . intersect ys . snd) (terms omega)
        combine (a,xs) = pairM (mul a) (xs++)



-- -- | Basic abstract 1-form
-- oneForm :: Ring f => Int -> Int -> Form f
-- oneForm i n | i < 0 || i > n = errForm "oneForm" MoProjBd
--             | otherwise       = Form 1 n [ (mulId,[i]) ]


-- TODO: shall we have something special for these? no need to state dimension
-- n since they will be constantly zero anyway

-- -- We need a basis here
-- inner :: (InnerProductSpace w, EuclideanSpace v, Dimensioned v, Scalar w ~ Scalar v)
--       => (Idx -> v -> Scalar w)  -- ^ Projection function in the specific vector space
--       -> Form w -> Form w -> Scalar w
-- inner proj omega eta
--     | degNEq omega eta = errForm "inner" BiDegEq -- TODO (??)
--     | otherwise = foldl
--           (flip $ \vs -> add (S.inner (app omega vs) (app eta vs)))
--           addId
--           (map pick' (permutations n (arity omega)))
--   where pick' is = pick (differences is) (map (unitVector n) [0..n-1])
--         app = refine proj
--         n = dimension omega


-- * Helper functions

-- | Checks arity equality
degNEq :: Form f -> Form f -> Bool
degNEq omega eta = arity omega /= arity eta

-- | Checks combined arity bound
degNBd :: Form f -> Form f -> Bool
degNBd  omega eta = (arity omega + arity eta) <= dimension omega

-- | Checks compatible underlying vector space dimensions between forms
spaNEq :: Form f -> Form f -> Bool
spaNEq omega eta = dimension omega /= dimension eta

-- | Checks compatible underlying vector space dimensions between a form and a
-- 'Dimensioned' type value
vecNEq :: Dimensioned v => Form f -> v -> Bool
vecNEq omega v = dimension omega /= dim v

errForm :: String -> FormMust -> t
errForm callee obligation = error $ "Form." ++ callee ++
                                    ": forms must " ++ show obligation



-- | Kinds of enforcements to the definitions and operations between/for 'Form'
data FormMust = BiDegEq | BiDegBd | BiSpaEq | MoProjBd | MoVecEq

instance Show FormMust where
  show BiDegEq  = "be of the same degree"
  show BiDegBd  = "have joint degree bounded by the working vector space dimension"
  show BiSpaEq  = "act on the same vector space"
  show MoProjBd = "project components of the underlying vector space"
  show MoVecEq  = "act on vectors of the working vectors space"
\end{code}

\section{Bibliography}

\bibliographystyle{plain}
\bibliography{doc/Bibliography}
