\section{Vectors}

The \module{Vector} implements vectors in $n$-dimensional euclidean space
 $\mathbf R^n$. In finite element exterior calculus, vectors are the elementary
unit of computation. Functions, alternating forms and differential forms all map
a single or a $k$-tuple of vectors into $\mathbf R$.

%------------------------------------------------------------------------------%

\ignore{
\begin{code}

{-# LANGUAGE TypeFamilies #-}

module FEEC.Internal.Vector(Vector(..),
              vector,
              toList,
              powV,
              dot,
              unitV,
              toPoint,
              fromPoint) where

import FEEC.Internal.Spaces hiding (toList)
import FEEC.Internal.Point
import FEEC.Utility.Print
import qualified FEEC.Internal.MultiIndex as MI
import Text.PrettyPrint
\end{code}
}

%------------------------------------------------------------------------------%

In Haskell, we use lists of type \code{Double} to represent vectors over
 $\mathbf R^n$.

\begin{code}
-- | Vectors in n-dimensional euclidean space.
data Vector = Vector [Double] deriving (Eq)

-- | R^n as a vector space.
instance VectorSpace Vector where
  type Fieldf Vector   = Double
  addV (Vector l1) (Vector l2) = Vector $ zipWith (+) l1 l2
  sclV c (Vector l) = Vector $ map (c*) l

-- | The dimension of vectors.
instance Dimensioned Vector where
    dim (Vector l) = length l
\end{code}

%------------------------------------------------------------------------------%

Vectors can be displayed on the command line in two ways. As an instance of
 \code{Show}, calling \code{show} on a vector will display the vector in
 Haskell structure. Using \code{pretty} to render the vector, will provide a
 visually more appealing output.

%------------------------------------------------------------------------------%

\begin{code}
instance RenderVector Vector where
    ncomps (Vector l) = length l
    components = toList

instance Show Vector where
    show v = "Vector:\n" ++ (show $ printVector 2 v)
\end{code}

%------------------------------------------------------------------------------%

The \module{Vector} module provides the \code{vector} functions for the smart
 construction of vectors. Even though right now the constructor does nothing
 except constructing the vector, the constructor is provided to hide the
 internal implementation to other modules.

%------------------------------------------------------------------------------%

\begin{code}

-- | Create vector from list of components.
vector :: [Double] -> Vector
vector = Vector

\end{code}

%------------------------------------------------------------------------------%
 Vectors can be manipulated using the \code{apply} function, that applies the
 given function to each element of the vector. For more complex computations,
 the components can be retrieved as a list using \code{toList}.
%------------------------------------------------------------------------------%

\begin{code}

-- | Apply the given functions to the elements of the vector.
apply :: Vector -> (Double -> Double) -> Vector
apply (Vector l) f = Vector (map f l)

-- | Return vector components as list.
toList :: Vector -> [Double]
toList (Vector l) = l

\end{code}

%------------------------------------------------------------------------------%

Points are used to represent position in n-dimensional euclidean space.
 Some computation require the conversion of points to vectors and vice versa.
 This can be done using the \code{toPoint} and \code{fromPoint} functions.

%------------------------------------------------------------------------------%

\begin{code}

-- | Point corresponding to given position vector.
toPoint :: Vector -> Point
toPoint (Vector l) = point l

-- | Position vector of given point.
fromPoint :: Point -> Vector
fromPoint (Point l) = Vector l

\end{code}

%------------------------------------------------------------------------------%

The function \code{dot} implements the dot product

\begin{align}
  u \cdot v = \sum_{i=0}^n u_iv_i
\begin{align}

of two vectors

\begin{align}
  u = \left [ \begin{array} u_1 \\ \ddots \\ u_n \end{array} \right ],
  v = \left [ \begin{array} v_1 \\ \ddots \\ v_n \end{array} \right ]
\end{align}

%------------------------------------------------------------------------------%

\begin{code}

-- | The dot product of two vectors.
dot :: Vector -> Vector -> Double
dot (Vector l1) (Vector l2) = foldl (\s (x,y) -> s + x*y) 0  $ zip l1 l2

\end{code}

%------------------------------------------------------------------------------%

The functions \code{unitV} creates the unit vector along the $i$th dimension in
$n$-dimensional euclidean space.

%------------------------------------------------------------------------------%

\begin{code}
-- | ith unit vector in R^n
unitV :: Int -- n
      -> Int -- i
      -> Vector
unitV n i = Vector $ concat [replicate i 0.0, [1.0], replicate (n-i-1) 0.0]

\end{code}

%------------------------------------------------------------------------------%

Polynomials in $n$ dimensions are linearcombinations of powers of vectors. The
 power of a vector up to a given multi-index
 $\alpha = (\alpha_0,\ldots,\alpha_{n-1})$ is defined as
\begin{align}
 v^\alpha =  \prod_{i=0}^{n-1} v_i^{\alpha_i}
\end{align}
The function \code{powV} implements powers of $n$-dimensional vectors.

%------------------------------------------------------------------------------%

\begin{code}

-- | Generalized power funciton for vectors. Given a list l of Int with
-- | the same length as the dimension of the vector v and components cs, the
-- | function computes the product of each component (cs!!i) raised to (l!!i)th
-- | power.
powV :: Vector -> MI.MultiIndex -> Double
powV (Vector cs) = powVList cs . MI.toList

powVList [] [] = mulId
powVList (v:vs) (i:is) = v ** fromIntegral i * powVList vs is
powVList _ _ = error "powV: Lists do not have equal length"
\end{code}
