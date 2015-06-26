\section{Simplex}

The \module{Simplex} module implements simplices in n-dimensional Euclidean
 space. A $k$-simplex in $n$-dimensional Euclidean space is the convex hull of
 $k+1$ simplices $v_0,\ldots,v_k$ such that $v_1-v_0,\ldots,v_k-v_0$ are
 linearly independent. $k$ is called the topological dimension of the simplex
 and $n$ its geometrical dimension. If $k=n$, we speak of a full simplex.

%------------------------------------------------------------------------------%

\ignore{
\begin{code}

{-# LANGUAGE
   GADTs,
   MultiParamTypeClasses,
   FlexibleContexts #-}

module FEEC.Internal.Simplex(
                             -- * The Simplex type
                             Simplex(..), simplex, simplex', referenceSimplex,

                             -- ** Properties
                             geometricalDimension, topologicalDimension,
                             volume,

                              -- * Subsimplices
                              subsimplex, subsimplices, subsimplices',
                              extendSimplex,

                             -- * Integration
                             integrateOverSimplex,

                             -- * Coordinates
                             cubicToBarycentric, barycentricToCartesian,
                             cubicToCartesian

                               ) where

import Data.List
import FEEC.Internal.Spaces

import FEEC.Internal.Point(Point, fromPoint, toPoint)
import qualified FEEC.Internal.Point as Point( unit, zero )

import FEEC.Internal.Vector
import FEEC.Utility.GramSchmidt
import FEEC.Utility.Print
import FEEC.Utility.Quadrature
import FEEC.Utility.Combinatorics
import FEEC.Utility.Utility
import qualified Numeric.LinearAlgebra.HMatrix as M

\end{code}

%------------------------------------------------------------------------------%

\subsection{The \code{Simplex} type}

In finite element exterior calculus we only consider full simplices and
 subsimplices of such. Therefore simplices are represented by an increasing
 list $\sigma$ and a list of vertices $v_0,\ldots,v_k$, with $k$ being the
 topological dimension of the simplex. For full simplices, $\sigma$ is just
 $0,\ldots,n$. For subsimplices of another simplex, $\sigma$ keeps track of
 which of the edges of the supersimplex is included in the subsimplex. This is
 needed for the extension of polynomials defined on a subsimplex of another
 simplex to the simplex itself.

%------------------------------------------------------------------------------%

\begin{code}
-- | n-simplex represented by a list of vectors of given dimensionality
-- | Invariant: geometrical dimension = length of the vector - 1
data Simplex =  Simplex { sigma :: [Int],
                          vertices :: [Point] }
                deriving (Eq, Show)

-- | The geometrical dimension of a simplex is the dimensionality of the
-- | underlying vector space.
geometricalDimension :: Simplex -> Int
geometricalDimension (Simplex _ (p:ps)) = dim p

-- | The topological dimension of a n-simplex is the number of vertices minus
-- | one.
topologicalDimension :: Simplex -> Int
topologicalDimension (Simplex _ l) = length l - 1

instance Pretty Simplex where
    pPrint t@(Simplex _ l) = int m <> text "-Simplex in "
                           <> rn n <> text ": \n"
                           <> printVectorRow 2 cs
        where cs = map (components . fromPoint) l
              n = geometricalDimension t
              m = topologicalDimension t

\end{code}

%------------------------------------------------------------------------------%

Two smart constructors are provided for the \code{Simplex} type. The
 \code{simplex} constructor creates a simplex from a given list of vertices and
 checks if geometrical and topological dimension of the simplex agree. The
 \code{simplex'} constructor creates a simplex from a given reference vertex
  $v_0$ and $n$ direction vectors $v_1-v_0,\ldots,v_n-v_0$.

%------------------------------------------------------------------------------%

\begin{code}

-- | Construct a full simplex from a given list of points in R^n
simplex :: [Point] -> Simplex
simplex l@(p:ps)
    | dim p == n = Simplex [0..n] l
    | otherwise = error "simplex: Dimensions don't agree."
    where n = length l
simplex [] = error "simplex: empty list."

-- | Construct a full simplex from a reference vertex and n direction vectors.
simplex' :: Point -> [Vector] -> Simplex
simplex' p0 l@(v:vs)
    | (dim p0 == dim v) && (dim p0 == n) = Simplex [0..n] (p0:l')
    | otherwise = error "simplex': Dimension don't agree."
    where l' = map (toPoint . addV (fromPoint p0)) l
          n = (length vs) + 1
simplex' _ _ = error "simplex': Space dimension is zero."

\end{code}

%------------------------------------------------------------------------------%

The reference simplex in $\R{n}$ is the simplex with vertices
\begin{align}
\left [ \begin{array} 0 \\ 0 \\ \cdots \\ 0 \end{array} \right ],
\left [ \begin{array} 1 \\ 0 \\ \cdots \\ 0 \end{array} \right ],
\left [ \begin{array} 0 \\ 1 \\ \cdots \\ 0 \end{array} \right ],
\ldots
\left [ \begin{array} 0 \\ 0 \\ \cdots \\ 1 \end{array} \right ]
\end{align}

%------------------------------------------------------------------------------%

\begin{code}
-- | Reference simplex in R^n
referenceSimplex :: Int -> Simplex
referenceSimplex n = Simplex [0..n] (Point.zero n : [Point.unit n i | i <- [0..n-1]])

\end{code}

%------------------------------------------------------------------------------%

The volume of a full simplex $T$ in n dimensions is given by
\begin{align}\label{eq:simplex_vol}
 V(T) &= \left | \frac{1}{n!}\text{det}\{v_1-v_0,\ldots,v_n-v_0\} \right |
\end{align}
If the topological dimension of the simplex is less than k, it is necessary to
 project the direction vectors $v_n-v_0,\ldots,v_1-v_0$ onto the space spanned
 by the subsimplex first.
The function \code{volume} checks whether the given simplex has full space
 dimension and if not performs the projection using \code{project} and
 \code{gramSchmidt}. \code{volume'} implements equation (\ref{eq:simplex_vol}).

%------------------------------------------------------------------------------%

\begin{code}

volume :: Simplex -> Double
volume t
    | k == n = volume' t
    | otherwise = volume (simplex' (Point.zero k) (project bs vs))
    where k = topologicalDimension t
          n = geometricalDimension t
          vs = directionVectors t
          bs = gramSchmidt vs

project :: [Vector] -> [Vector] -> [Vector]
project bs vs = map vector [[ dot b v | b <- bs] | v <- vs]

-- | Computes the k-dimensional volume (Lebesgue measure) of a simplex
-- | in n dimensions using the Gram Determinant rule.
volume' :: Simplex -> Double
volume' t = sqrt (abs (M.det w)) / fromInteger (factorial n)
    where n = geometricalDimension t
          w = M.matrix n (concatMap components (directionVectors t))


\end{code}

%------------------------------------------------------------------------------%

\subsection{Subsimplices}

A subsimplex, or face, $f$ of dimension $k$ of a simplex $v_0,\ldots,v_n$ is any
 simplex consisting of a subset $v_{i_0},\ldots,v_{i_k}$ of the vertices
 $v_0,\ldots,v_n$. A $k$-subsimplex of a given simplex may be identified by an
 increasing map $\sigma$. Using lexicographic ordering of the increasing maps of
 a given length it is possible to define an ordering over the simplices. This
 allows us to index each subsimplex of a given dimension $k$ of a simplex. Three
 functions are provided to obtain subsimplices from a given simplex.
 \code{subsimplex} returns the $i$th $k$-subsimplex of a given simplex.
 \code{subsimplices} returns a list of all the subsimplices of dimension $k$ and
 \code{subsimplices'} returns a list of all the subsimplices of dimension $k$ or
 higher.

%------------------------------------------------------------------------------%

\begin{code}
-- | i:th k-dimensional subsimplex of given simplex
subsimplex :: Simplex -> Int -> Int -> Simplex
subsimplex (Simplex _ []) _ _ =
                error "subsimplex: Encountered Simplex without vertices."
subsimplex s@(Simplex _ l) k i
           | k > n = error err_dim
           | i >= (n+1) `choose` (k+1) = error err_ind
           | otherwise = Simplex indices (map (l !!) indices)
    where n = topologicalDimension s
          indices = unrank (k+1) n i
          err_ind = "subsimplex: Index of subsimplex exceeds (n+1) choose (k+1)."
          err_dim = "subsimplex: Dimensionality of subsimplex is higher than that of the simplex."

-- | List subsimplices of given simplex with dimension k.
subsimplices :: Simplex -> Int -> [Simplex]
subsimplices t@(Simplex _ l) k
             | k > n = error err_dim
             | otherwise = [Simplex i vs | (i, vs) <- zip indices subvertices]
    where n = topologicalDimension t
          indices = map (unrank (k+1) n) [0..(n+1) `choose` (k+1) - 1]
          subvertices = map (takeIndices l) indices
          err_dim = "subsimplices: Dimensionality of subsimplices is higher than that of the simplex."

-- | List subsimplices of given simplex with dimension larger or equal to k.
subsimplices' :: Simplex -> Int -> [Simplex]
subsimplices' t k = concat [ subsimplices t k' | k' <- [k..n] ]
    where n = topologicalDimension t

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
extendSimplex :: Simplex -> Simplex
extendSimplex t@(Simplex _ ps)
              | n == nt = t
              | otherwise = simplex' p0 (take n (extendVectors n dirs))
    where n = geometricalDimension t
          nt = topologicalDimension t
          dirs = directionVectors t
          p0 = referenceVertex t

-- | Uses the Gram-Schmidt method to add at most n orthogonal vectors to the
-- | given set of vectors. Due to round off error the resulting list may contain
-- | more than n vectors, which then have to be removed manually.
extendVectors :: Int -> [Vector] -> [Vector]
extendVectors n vs = sort vs'
    where vs' = gramSchmidt' vs [unitVector n i | i <- [0..n-1]]


-- | Reference vertex of the simplex, i.e. the first point in the list
-- | of vectors
referenceVertex :: Simplex -> Point
referenceVertex (Simplex _ (p:ps)) = p

-- | List of the n direction vector pointing from the first point of the
-- | simplex to the others.
directionVectors :: Simplex -> [Vector]
directionVectors (Simplex _ (p:ps)) = map (\x -> subV (fromPoint x) v) ps
    where v = fromPoint p
directionVectors (Simplex _ _) = []

\end{code}

%------------------------------------------------------------------------------%

\subsection{Integration}

For integration of arbitrary functions over a simplex $T$ we use the method
 proposed in \cite{integrals}. Using the Duffy transform \cite{duffy}, the
 integral over the simplex is transformed into an integral over the
 $n$-dimensional unit cube.

\begin{align}
  \int_T f(\vec{x}) \: dx &= n!|T| \int_0^1 dt_1(1-t_1)^{n-1}\ldots\int_0^1 dt_nf(\vec{t})
\end{align}

The additional factors $(1-t_i)^{n-i}$ can be absorbed into the quadrature rule
 by using a Gauss-Jacobi quadrature

\begin{align}
  \int_0^1 dt_i(1-t_i)^\alpha = \sum_{j=0}^q w^\alpha_j f(\xi^\alpha_j)
\end{align}

where $alpha=n-i$. The \xi^\alpha_j are the roots of the Jacobi polynomial
 $P_q^{\alpha,0}$ and the w^\alpha_j the corresponding weights, that can be
 computed using the Golub-Welsch algorithm\cite{GolubWelsch}. The integral of
 $f$ over $T$ can then be approximated using

\begin{align}\label{eq:integral}
  \int_T f(\vec{x}) \: dx &= n!|T| \\sum_{j_1=0}^q w^{n-1}_{j_1}\ldots\sum_j{j_n=0}^qw_{j_n}f(\xi^0_{j_n})
\end{align}

The function \code{integral} uses (\ref{eq:integral}) to approximate the
 integral of an arbitrary function over a simplex. The computation of the
  weights is performed by the \module{quadrature} module.

%------------------------------------------------------------------------------%

\begin{code}

-- | Numerically integrate the function f over the simplex t using a Gauss-Jacobi
-- | quadrature rule with q nodes.
integrateOverSimplex :: (Function f Vector, Values f Vector ~ Double)
                     => Int     -- q
                     -> Simplex -- t
                     -> f
                     -> Double
integrateOverSimplex q t f = vol * fac * (nestedSum q (n-1) [] t f)
    where n   = topologicalDimension t
          fac = fromInteger (factorial n)
          vol = volume t

-- Recursion for the computation of the nested integral as given in formula (3.6)
-- in "Bernstein-Bezier Finite Elements of Arbitrary Order and Optimal Assembly
-- Procedues" by Ainsworth et al.
-- TODO: Use points for evaluation, return suitable values from quadrature.
nestedSum :: (Function h Vector, Values h Vector ~ Double)
             => Int -> Int -> [Double] -> Simplex -> h -> Double
nestedSum k d ls t f
          | d == 0 = sum [ w * (eval x f ) | (w, x) <- zip weights xs ]
          | otherwise = sum [ w * (nestedSum k (d-1) (x:ls) t f) | (w, x) <- zip weights nodes ]
    where xs = map fromPoint [ cubicToCartesian t (P.point (xi : ls)) | xi <- nodes ]
          (nodes, weights) = unzip $ gaussJacobiQuadrature d 0 k

\end{code}

%------------------------------------------------------------------------------%

\subsection{Coordinates}
\label{sec:Coordinates}

In \FEEC, we work with three kinds of coordinates: Cartesian
 coordinates, barycentric coordinates and cubic coordinates. In barycentric
 coordinates, a point inside a simplex $v_0,ldots,v_n$ is given by a convex
 combination of its vertices. That is, a point $p$ is given in
 barycentric coordinates by a tuple (\lambda_0,\ldots,\lambda_n) if

\begin{align}
  \mathbf p &= \lambda_0v_0 + \ldots + \lambda_nv_n
\end{align}

The barycentric coordinates of a point inside a simplex are either positive or
 zero and sum to one. Moreover, the barycentric coordinates of the vertices of
 the simplex are given by

\begin{align}
  v_0 = (1,0,\ldots,0), v_1 = (0,1,\ldots,0),\ldots,v_n=(0,0,\ldots,1)
\end{align}

A mapping between points in the n-dimensional unit cube
 $t = (t_0,\ldots,t_{n-1})$ and the barycentric coordinates of a simplex
 is defined by

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

The functions \code{barycentricToCartesian}, \code{cubicToCartesian} and
 \code{cubicToBarycentric} convert between the different coordinates systems.
 Note that barycentric and cubic coordinates define a point in space only with
 respect to a given simplex.

%------------------------------------------------------------------------------%

\begin{code}

-- | Convert a point given in barycentric coordinates to euclidean coordinates.
barycentricToCartesian :: Simplex -> Point -> Point
barycentricToCartesian t p = toPoint $ barycentricToCartesian' t (fromPoint p)

-- | Convert a vector given in barycentric coordinates to euclidean coordinates.
barycentricToCartesian' :: Simplex -> Vector -> Vector
barycentricToCartesian' t@(Simplex _ ps) v = foldl sclAdd zero (zip v' vs')
    where sclAdd p (c, p0) = addV p (sclV c p0)
          zero             = zeroV v
          vs'              = map fromPoint ps
          v'               = components v
          n                = geometricalDimension t

-- | The inverse Duffy transform. Maps a point from the unit cube in R^{n+1}
-- | to Euclidean space.
cubicToCartesian :: Simplex -> Point -> Point
cubicToCartesian t = (barycentricToCartesian t) . cubicToBarycentric

-- | Convert point given in cubic coordinates to barycentric coordinates.
cubicToBarycentric :: Point -> Point
cubicToBarycentric = toPoint . cubicToBarycentric' . fromPoint

-- | Convert vector given in cubic coordinates to barycentric coordinates.
cubicToBarycentric' :: Vector -> Vector
cubicToBarycentric' v = vector (ls ++ [l])
    where ts = components v
          (l,ls) = mapAccumL f 1 ts
          f acc t = (acc * (1 - t), t * acc)

\end{code}

%------------------------------------------------------------------------------%

