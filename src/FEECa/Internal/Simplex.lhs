\section{Simplex}

The \inlcode{Simplex} module implements simplices in $n$-dimensional
Euclidean space $\R{n}$.
%
A $k$-simplex $\smp{T} = [\vec{v_0},\ldots,\vec{v_k}]$ in
$n$-dimensional Euclidean space $\R{n}$ is the convex hull of $k+1$
vertices $\vec{v_0},\ldots,\vec{v_k}$ such that the spanning vectors
$\vec{v_1}-\vec{v_0} ,\ldots,\vec{v_k}-\vec{v_0}$ are linearly
independent.
%
The \textit{topological dimension} of a simplex is the number $k$ of
vectors spanning the simplex.
%
The \textit{geometrical dimension} of a simplex is the dimension $n$
of the underlying space $\R{n}$.
%
If $k=n$, we speak of a full simplex.
%
A subsimplex of a given simplex
$\smp{T} = [\vec{v_0},\ldots,\vec{v_k}]$ is a simplex
$\smp{T'} = [\vec{v_{i_0}},\ldots,\vec{v_{i_{k'}}}]$ such that
$\{i_0,\ldots,i_{k'}\} \subset \{1,\ldots,k\}$.
%
Such a subsimplex can be conveniently represented using a map
$\sigma: \{0,\ldots,k'\} \to \{0,\ldots,k\}$ such that
$\sigma(j) = i_j$ for all $j=0,\ldots,k'$.
%
For the representation to be unique, we require $\sigma$ to be
increasing, i.e.\  $\sigma(j+1) > \sigma(j)$ for all $j=1,\ldots,k'-1$.

%------------------------------------------------------------------------------%

\begin{code}

{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}

module FEECa.Internal.Simplex (
  -- * The Simplex type
  Simplex(..), simplex, simplex', referenceSimplex, face,

  -- ** Properties
  geometricalDimension, topologicalDimension, volume, spanningVectors,
  referenceVertex, extendVectors,

  -- * Subsimplices
  subsimplex, subsimplices, subsimplices', extendSimplex, complement,
  boundary, boundarySigns,

  -- * Integration
  integrateOverSimplex,

  -- * Coordinates
  cubicToBarycentric, barycentricToCartesian, cubicToCartesian
  ) where

import Prelude    hiding  ( (<>) )
import Data.List
import qualified  Numeric.LinearAlgebra.HMatrix as M

import FEECa.Internal.Spaces
import FEECa.Utility.Combinatorics
import FEECa.Utility.GramSchmidt
import FEECa.Utility.Print
import FEECa.Utility.Quadrature
import qualified  FEECa.Utility.Utility         as U


\end{code}


%------------------------------------------------------------------------------%

\subsection{The \incode{Simplex} type}

 We represent a simplex $\smp{T} = [\vec{v_0},\ldots,\vec{v_k}]$ by a list containing
 the vertices $\vec{v_0},\ldots,\vec{v_k}$ and an increasing map $\sigma$
 keeping track of which vertices of a potential super-simplex the simplex
 contains. The mapping $\sigma$ is represented by an increasing list
 $\sigma(1),\ldots, \sigma(k)$.

 The \incode{Simplex} type is parametrized by the type used to represent vectors
 in $\R{n}$. The type \incode{Simplex a} contains two fields.
 \incode{sigma :: [Int]} represents the mapping $\sigma$ and
 \incode{vertices :: [a]} the vertices of the simplex. For full simplices,
 $\sigma$ is just $0,\ldots,n$. For subsimplices of another simplex, $\sigma$
 keeps track of which of the vertices of the supersimplex are included in the
 subsimplex. This is needed for the extension of polynomials defined on a
 subsimplex of another simplex to the simplex itself.

%------------------------------------------------------------------------------%

\begin{code}
-- | n-simplex represented by a list of vectors of given dimensionality
-- | Invariant: geometrical dimension = length of the vertices - 1
data Simplex a =  Simplex { sigma :: [Int],
                            vertices :: [a] }
  deriving (Eq, Show)

-- | The geometrical dimension of a simplex is the dimensionality of the
-- | underlying vector space.
geometricalDimension :: Dimensioned a => Simplex a -> Int
geometricalDimension (Simplex _ (p:_)) = dim p
geometricalDimension _                 = error "geometricalDimension: empty list of vertices"

-- | The topological dimension of a n-simplex is the number of vertices minus
-- | one.
topologicalDimension :: Simplex a -> Int
topologicalDimension (Simplex _ l) = length l - 1

\end{code}

%------------------------------------------------------------------------------%

 We call the first vertex $\vec{v_0}$ of a simplex
 $\smp{T} = [\vec{v_0},\ldots,\vec{v_k}]$ as its reference vertex. The vectors
 $\vec{v_1}-\vec{v_0},\ldots,\vec{v_n}-\vec{v_0}$. are referred to as the
 simplices spanning vectors.

%------------------------------------------------------------------------------%

\begin{code}
-- | Reference vertex of the simplex, i.e. the first point in the list
-- | of vectors
referenceVertex :: Simplex v -> v
referenceVertex (Simplex _ (p:_)) = p
referenceVertex _                 = error "referenceVertex: there is no vertex in this simplex"

-- | List of the n direction vectors pointing from the first point of the
-- | simplex to the others.
spanningVectors :: VectorSpace v => Simplex v -> [v]
spanningVectors (Simplex _ (v:vs)) = map (`subV` v) vs
spanningVectors (Simplex _ _)      = []

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Printing Simplices}

For printing of simplices two methods are provided. As an instance of the
\incode{Show} class¸ \incode{show} displays the internal structure of the
\incode{Simplex} type. \incode{pPrint} renders the simplex as a list of vectors
using unicode.

%------------------------------------------------------------------------------%

\begin{code}
instance EuclideanSpace v => Pretty (Simplex v) where
  pPrint t@(Simplex _ l) = int m <> text "-Simplex in "
                            <> rn n $+$ -- <> text ":\n"
                            printVectorRow 2 cs
    where cs = map toDouble' l
          n  = geometricalDimension t
          m  = topologicalDimension t

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Constructors}

 The \incode{Simplex} type provides two different constructors for the
 construction of full simplices. The \incode{simplex} constructor creates a
 simplex from a given list of vertices. The \incode{simplex'} constructor creates
 a simplex from a given reference vertex $\vec{v_0}$ and $n$ direction vectors
 $\vec{v_1}-\vec{v_0},\ldots,\vec{v_n}-\vec{v_0}$. In both cases the topological
 dimension of the simplex must agree with the geometrical one, otherwise an
 error is thrown.

%------------------------------------------------------------------------------%

\begin{code}

-- | Construct a full simplex from a given list of points in R^n
simplex :: EuclideanSpace v => [v] -> Simplex v
simplex []                = error "simplex: empty list."
simplex l@(p:_)
    | all ((n==) . dim) l = Simplex [0..n] l
    | otherwise           = error "simplex: Dimensions don't agree."
  where n = length l - 1

-- | Construct a full simplex from a reference vertex and n direction vectors.
simplex' :: EuclideanSpace v => v -> [v] -> Simplex v
simplex' _  [] = error "simplex': Topological dimension is zero."
simplex' p0 vs = simplex (p0 : map (addV p0) vs)

\end{code}

%------------------------------------------------------------------------------%

 The function \incode{referenceSimplex} constructs the reference simplex in
 $\R{n}$, which is defined as the simplex $T=[\vec{v_0},\ldots,\vec{v_n}]$ with
 vertices
\begin{align}
  \vec{v_0} =
\left [ \begin{array}{c} 0 \\ 0 \\ \vdots \\ 0 \end{array} \right ],
  \vec{v_1} =
\left [ \begin{array}{c} 1 \\ 0 \\ \vdots \\ 0 \end{array} \right ],
  \vec{v_2} =
\left [ \begin{array}{c} 0 \\ 1 \\ \vdots \\ 0 \end{array} \right ],
\ldots,
  \vec{v_n} =
\left [ \begin{array}{c} 0 \\ 0 \\ \vdots \\ 1 \end{array} \right ]
\end{align}

%------------------------------------------------------------------------------%

\begin{code}
-- | Reference simplex in R^n
referenceSimplex :: EuclideanSpace v => Int -> Simplex v
referenceSimplex n = Simplex [0..n] (zero n : [unitVector n i | i <- [0..n-1]])

face :: Simplex v -> [Int] -> Simplex v
face (Simplex sigma1 vs) sigma2 = Simplex [sigma1 !! i | i <- sigma2]
                                          [vs !! i     | i <- sigma2]
\end{code}

%------------------------------------------------------------------------------%

  \subsection{Volume of a Simplex}

 The volume of a full simplex $\smp{T}$ in n dimensions is given by
 \begin{align}\label{eq:simplex_vol}
  V(\smp{T}) &=
  \left | \frac{1}{n!}\text{det}\{\vec{v_1}-\vec{v_0},
                                  \ldots,
                                  \vec{v_n}-\vec{v_0}\} \right |
 \end{align}
 If the topological dimension of the simplex is less than k, it is necessary to
 project the spanning vectors $v_n-v_0,\ldots,v_1-v_0$ onto the space spanned
 by the subsimplex first. This is done by first constructing an orthonormal
 basis of the space using the Gram-Schmidt method and then projecting the
 spanning vectors onto that space.

 The function \incode{volume} checks whether the given simplex has full space
 dimension and if not performs the projection using \incode{project} and
 \incode{gramSchmidt}. \incode{volume'} simply uses the above formula to compute the
 volume of a full simplex $\smp{T}$.

%------------------------------------------------------------------------------%

\begin{code}

-- | Computes the k-dimensional volume (Lebesgue measure) of a simplex
-- | in n dimensions using the Gram Determinant rule.
volume :: EuclideanSpace v
       => Simplex v -> Scalar v
volume t
    | k == n    = volume' t
    | otherwise = volume (simplex' (zero k) (project bs vs))
  where k  = topologicalDimension t
        n  = geometricalDimension t
        vs = spanningVectors t
        bs = gramSchmidt vs

project :: EuclideanSpace v => [v] -> [v] -> [v]
project bs vs = [ fromList [proj b v | b <- bs] | v <- vs]
  where proj b v  = divide (dot b v) (sqrt' (dot b b))
        sqrt'     = fromDouble . sqrt . toDouble

volume' :: EuclideanSpace v => Simplex v -> Scalar v
volume' t = fromDouble (abs (M.det w)) `divide` embedIntegral (factorial n)
  where n = geometricalDimension t
        w = M.matrix n comps
        comps = concatMap toDouble' (spanningVectors t)

\end{code}

%------------------------------------------------------------------------------%

\subsection{Subsimplices}

 A subsimplex, or face, $\smp{f}$ of dimension $k$ of a simplex
 $\smp{T} = [\vec{v_0},\ldots,\vec{v_n}]$ is any simplex consisting of a subset
 $\vec{v_{i_0}},\ldots,\vec{v_{i_k}}$ of the vertices $v_0,\ldots,v_n$. Such a
 $k$-subsimplex of a given simplex \smp{T} may be identified by an increasing
 map $\sigma$. Using lexicographic ordering of the increasing maps of a given
 length it is possible to define an ordering over the simplices. This allows us
 to index each subsimplex of a given dimension $k$ of a simplex.

 Three functions are provided to obtain subsimplices from a given simplex.
\incode{subsimplex} returns the $i$th $k$-subsimplex of a given simplex.
 \incode{subsimplices} returns a list of all the subsimplices of dimension $k$ and
 \incode{subsimplices'} returns a list of all the subsimplices of dimension $k$ or
 higher.

%------------------------------------------------------------------------------%

\begin{code}
-- | i:th k-dimensional subsimplex of given simplex
subsimplex :: Simplex v -> Int -> Int -> Simplex v
subsimplex (Simplex _ []) _ _ =
      error "subsimplex: Encountered Simplex without vertices."
subsimplex s@(Simplex _ l) k i
    | k > n                     = error err_dim
    | i >= (n+1) `choose` (k+1) = error err_ind
    | otherwise                 = Simplex indices (U.takeIndices l indices)
  where n = topologicalDimension s
        indices = unrank (k+1) n i
        err_ind = "subsimplex: Index of subsimplex exceeds (n+1) choose (k+1)."
        err_dim = "subsimplex: Dimensionality of subsimplex is higher than"
                  ++ " that of the simplex."

-- | List subsimplices of given simplex with dimension k.
subsimplices :: Simplex v -> Int -> [Simplex v]
subsimplices t@(Simplex _ l) k
    | k > n     = error err_dim
    | otherwise = [ Simplex ix (U.takeIndices l ix) |
                      ix <- increasingLists (k+1) n ]
  where n       = topologicalDimension t
        err_dim = "subsimplices: Dimensionality of subsimplices is"
                  ++ "higher than that of the simplex."
-- TODO: note that the implementation has always seemed to ignore the original
--  "permutation" of indices we had from the original simplex if the argument
--  happens to be a subsimplex, ie: non-full simmplex. Now, should we check or
--  is it intended functionality?

-- | List subsimplices of given simplex with dimension larger or equal to k.
subsimplices' :: Simplex v -> Int -> [Simplex v]
subsimplices' t k = concat [ subsimplices t k' | k' <- [k..n] ]
  where n = topologicalDimension t

\end{code}

%------------------------------------------------------------------------------%

\begin{code}
complement :: Simplex v -> Simplex v -> [v]
complement (Simplex _ l) (Simplex sigm _) = [l !! i | i <- [0..n], i `notElem` sigm]
    where n = length l - 1

boundary :: Simplex v -> [Simplex v]
boundary t
    | k <= 0    = error err_dim
    | otherwise = subsimplices t (k-1)
  where k = topologicalDimension t
        err_dim = "boundary: Can't take the boundary of a simplex with topological dimension "
                  ++ "less than 1."

boundarySigns :: EuclideanSpace v => Simplex v -> [Scalar v]
boundarySigns t
    | k <= 0    = error err_dim
    | otherwise = map (\x -> if even x then mulId else addInv mulId) [0..k]
  where k = topologicalDimension t
        err_dim = "boundary: Can't take the boundary of a simplex with topological dimension "
                  ++ "less than 1."
\end{code}

%------------------------------------------------------------------------------%

For the computation of the barycentric coordinates of a simplex whose
 topological dimension is smaller than its geometrical dimension it is necessary
 to extend the simplex to a full simplex. For a given $k$-subsimplex in $n$
 dimensions this is done by adding $n-k$ vertices $v_{k+1},\ldots,v_{n}$ such
 that the vectors $v_{k+1}-v_0,\ldots,v_{n}-v_0$ are orthogonal mutually as well
 as to the vectors $v_{1}-v_{0},\ldots,v_{k}-v_{0}$.

%------------------------------------------------------------------------------%

\begin{code}
-- | Extend the given simplex to a full simplex so that its geometrical
-- | dimension is the same as its topological dimension.
extendSimplex :: EuclideanSpace v => Simplex v -> Simplex v
extendSimplex t
    | n == nt   = t
    | otherwise = simplex' p0 (extendVectors n dirs)
  where n     = geometricalDimension t
        nt    = topologicalDimension t
        dirs  = spanningVectors t
        p0    = referenceVertex t

-- | Uses the Gram-Schmidt method to add at most n orthogonal vectors to the
-- | given set of vectors. Due to round off error the resulting list may contain
-- | more than n vectors, which then have to be removed manually.
extendVectors :: EuclideanSpace v
              => Int -> [v] -> [v]
extendVectors n [] = extendOrthGS (Right n)
extendVectors _ vs = extendOrthGS (Left vs)
    {- XXX: this was there before, jointly with providing GS with the
            n unit vectors in addition to vs, but it seems to be quite
            unnecessary; the comment in the function description doesn't seem
            to apply since we know at most n-k additional vectors could ever
            form an orthogonal basis jointly with vs; changed it so that GS
            takes care of this, as would be expected (ie: it will give at most
            n orthogonal vectors where n = dim (EuclideanSpace v):
        take (n - k)  (U.sortOn (Down . norm2) us
      where us = {-drop k $ -}gramSchmidt $ vs ++ [unitVector n i | i <- [0..(n-1)]]
            k  = length vs -}
\end{code}

%------------------------------------------------------------------------------%

\subsection{Coordinates}
\label{sec:Coordinates}

 In exterior calculus we will be working with two different coordinates systems:
 Standard cartesian coordinates and barycentric coordinates of a given simplex.
 Moreover, for the computation of integrals over a simplex we will use an
 additional set of coordinates to which we will refer to as cubic coordinates.

\subsubsection{Barycentric Coordinates}

 In barycentric coordinates, a point inside a simplex
 $\smp{T} = [\vec{v_0},\ldots,\vec{v_n}]$ is given by a convex combination of its
 vertices. That is, a point $\vec{p}$ is given in barycentric coordinates by a
 tuple $(\lambda_0,\ldots,\lambda_n)$ if

 \begin{align}
   \vec{p} &= \lambda_0\vec{v_0} + \ldots + \lambda_n\vec{v_n}
 \end{align}

 The barycentric coordinates of a point inside a simplex are either positive or
 zero and sum to one. Moreover, the barycentric coordinates of the vertices of
 the simplex are given by

 \begin{align}
   \vec{v_0} = (1,0,\ldots,0), \vec{v_1} = (0,1,\ldots,0),\ldots,
   \vec{v_n}=(0,0,\ldots,1)
 \end{align}

 The function \inlcode{barycentricToCartesian} converts a vector given in
 barycentric coordinates to cartesian coordinates:

%------------------------------------------------------------------------------%

\begin{code}

-- | Convert a vector given in barycentric coordinates to euclidean coordinates.
barycentricToCartesian :: EuclideanSpace v
                       => Simplex v -> v -> v
barycentricToCartesian (Simplex _ vs) v = U.sumV (zipWith sclV (toList v) vs)
\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Cubic Coordinates}

 A mapping between points in the n-dimensional unit cube
 $t = (t_0,\ldots,t_{n-1}) \in [0,1]^{n}$ and the barycentric coordinates of a
 simplex $\smp{T}$ can be defined using

\begin{subequations}
  \begin{align}
    \lambda_0 &= t_0 \\
    \lambda_1 &= t_1(1 - \lambda_0) \\
    \lambda_2 &= t_2(1 - \lambda_1 - \lambda_0) \\
    & \vdots \\
    \lambda_{n-1} &= t_{n-1}(1 - \lambda_{n-1} - \cdots - \lambda_0) \\
    \lambda_{n} &= 1 - \lambda_{n-1} - \cdots - \lambda_0
  \end{align}
\end{subequations}

 We refer to the tuple $(t_0,\ldots,t_{n-1})$ as the cubic coordinates of the
 corresponding point $(\lambda_0,\ldots,\lambda_n)$ in barycentric coordinates.

 The functions \inlcode{cubicToCartesian} and \inlcode{cubicToBarycentric} convert
 from the cubic coordinate system to Cartesian and barycentric coordinates,
 respectively. Note that cubic coordinates define a point in space only with
 respect to a given simplex.

%------------------------------------------------------------------------------%

\begin{code}
-- | The inverse Duffy transform. Maps a point from the unit cube in R^{n+1}
-- | to Euclidean space.
cubicToCartesian :: EuclideanSpace v => Simplex v -> v -> v
cubicToCartesian t = barycentricToCartesian t . cubicToBarycentric

-- | Convert vector given in cubic coordinates to barycentric coordinates.
cubicToBarycentric :: EuclideanSpace v => v -> v
cubicToBarycentric v = fromList (ls ++ [l])
  where ts      = toList v
        (l,ls)  = mapAccumL f mulId ts
        f acc t = (mul acc (sub mulId t), mul t acc)

\end{code}

%------------------------------------------------------------------------------%


%------------------------------------------------------------------------------%

\subsection{Integrals over Simplices}

 For the integration of a function $f$  over a simplex $\smp{T}$ we use the
 method proposed in \cite{Ainsworth}. Performing a change of coordinates from
 Cartesian to cubic coordinates, the integral over the simplex takes the form

 \begin{align}
   \int_\smp{T} d\vec{x} \: f(\vec{x}) \:  &=
   V(\smp{T}) \int_0^1 dt_1(1-t_1)^{n-1}\ldots\int_0^1 dt_nf(\vec{x}(t_0,\ldots,t_{n-1}))
 \end{align}

 where $V(\smp{T})$ is the volume of the simplex. For a numerical approximation
 of the integral, the factors $(1-t_i)^{n-i}$ can be absorbed into the
 quadrature rule by using a Gauss-Jacobi quadrature

\begin{align}
  \int_0^1 dt_i(1-t_i)^\alpha = \sum_{j=0}^q w^\alpha_j f(\xi^\alpha_j)
\end{align}

where $\alpha=n-i$. The points $\xi^\alpha_j$ are the roots of the Jacobi
polynomial $P_q^{\alpha,0}$ and the $w^\alpha_j$ the corresponding weights, that
can be computed using the Golub-Welsch algorithm \cite{GolubWelsch}. The
integral of $f$ over $\smp{T}$ can then be approximated using

\begin{align}\label{eq:integral}
  \int_\smp{T} d\vec{x} \: f(\vec{x}) \:&=
  V(\smp{T}) \sum_{j_1=0}^q w^{n-1}_{j_1}\ldots \sum_{j_n=0}^qw_{j_n}f(\xi^0_{j_n}) \end{align}

 The above formula is implemented by the function \inlcode{integral}. The
 computation of the quadrature weights and nodes is implemented in
 \inlcode{FEECa.Utility.Quadrature}.

%------------------------------------------------------------------------------%

\begin{code}

-- | Numerically integrate the function f over the simplex t using a Gauss-Jacobi
-- | quadrature rule with q nodes.
integrateOverSimplex :: (EuclideanSpace v, r ~ Scalar v)
                     => Int             -- q
                     -> Simplex v       -- t
                     -> (v -> r)        -- f
                     -> r
integrateOverSimplex q t f = mul vol (nestedSum q t f (n-1) [])
  where n   = topologicalDimension t
        vol = volume t

-- Recursion for the computation of the nested sum in the numerical approximation
-- of the integral of a function over a simplex.
nestedSum :: (EuclideanSpace v, r ~ Scalar v)
             => Int -> Simplex v -> (v -> r) -> Int -> [r] -> r
nestedSum k t f d ls
    | d == 0    = U.sumR [ mul w fx
                            | (xi,a) <- quad,
                              let w   = fromDouble a,
                              let fx  = xNode (f.nodeCoord) xi ]
                        -- [ mul w (f x)
                        --    | (w, x) <- zip weights' xs ]
    | otherwise = U.sumR $ map (uncurry mul . U.pairM (xNode (nestedSum k t f (d-1))) fromDouble) quad
                        -- [ mul w (nestedSum k (d-1) (x:ls) t f)
                        --    | (w, x) <- zip weights' nodes' ]
  where nodeCoord = cubicToCartesian t . fromList'
        -- xs        = [ (cubicToCartesian t (fromList' (xi : ls)) , w) | (xi, w) <- quad ]
        fromList' = fromList . reverse
        -- TODO: remove list reversal above via a local declaration + accum param
        --      + generalise according to the commong ground found above (via xNode)
        -- wMul      = mul . fromDouble
        xNode h   = h . (:ls) . fromDouble
        quad      = gaussJacobiQuadrature' d 0 k
        -- (nodes, weights) = unzip $ gaussJacobiQuadrature' d 0 k
        -- nodes     = map (fromDouble.fst) quad
        -- weights   = map (fromDouble.snd) quad

instance (EuclideanSpace v, r ~ Scalar v) => FiniteElement (Simplex v) r where
  type Primitive (Simplex v) = v
  quadrature = integrateOverSimplex

\end{code}

\section{Bibliography}

\bibliographystyle{plain}
\bibliography{doc/Bibliography}
