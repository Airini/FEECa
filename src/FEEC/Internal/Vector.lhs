\section{Vectors}

The \module{Vector} implements vectors in $n$-dimensional euclidean space
 $\mathbf R^n$. Vectors, as opposed to points, describe directions in space.
Vectors are used for the evaluation of alternating forms.

%------------------------------------------------------------------------------%

\ignore{
\begin{code}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FEEC.Internal.Vector(

  -- * The Vector Type
  Vector(components), Dimensioned(..), vector,

  -- * Manipulating Vectors
  apply, toList,

  -- * Convenience functions

  -- * Mathematical Functions
  dot, pow

  ) where

import FEEC.Internal.Spaces
import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Utility.Print
import qualified FEEC.Utility.Utility as U


\end{code}
}

%------------------------------------------------------------------------------%

 Vectors in $\R{n}$ are represented using lists of type \code{Double}. The
 dimension $n$ is inferred from the length of the list. Since vectors from
 Euclidean spaces of different dimension are of the same type, it is up to the
 user not to mix vectors from different spaces, which will lead to runtime errors.

\begin{code}
-- instance (Eq a, Num a) => Ring a where
--   add = (+)
--   addId = 0
--   addInv = (0-)
--   mul = (*)
--   mulId = 1
--   fromInt = fromIntegral

-- instance (Eq a, Fractional a) => Field a where
--     mulInv a = 1/a

-- | Vectors in n-dimensional euclidean space.
data Vector a = Vector { components :: [a] } deriving (Show)

instance Eq (Vector Double) where
    v1 == v2 = and (zipWith U.eqNum (components v1) (components v2))

instance Eq (Vector Rational) where
    v1 == v2 = and (zipWith (==) (components v1) (components v2))

-- | R^n as a vector space.
instance Ring a => VectorSpace (Vector a) where
  type Scalar (Vector a) = a
  addV (Vector l1) (Vector l2) = Vector $ zipWith add l1 l2
  sclV c (Vector l) = Vector $ map (mul c) l

-- | R^n as a vector space.
instance (Field a, Eq (Vector a)) => EuclideanSpace (Vector a) a where
    dot (Vector l1) (Vector l2) = foldl (\s (x,y) -> s + x*y) 0  $ zip l1 l2
    toList   = components
    fromList = vector

-- | The dimension of vectors.
instance Dimensioned (Vector a) where
    dim (Vector l) = length l

-- | Comparing vectors by length.
-- instance (Field a, Ord a) => Ord (Vector a) where
--     v1 <= v2 = (dot v1 v1) <= (dot v2 v2)

\end{code}

%------------------------------------------------------------------------------%

Vectors can be displayed on the command line in two ways. As an instance of
 \code{Show}, calling \code{show} on a vector will display the vector in
 Haskell structure. Using \code{pretty} to render the vector, will provide a
 visually more appealing output.

%------------------------------------------------------------------------------%

\begin{code}
instance (Field a, RealFrac a) => Pretty (Vector a) where
    pPrint v = text "Vector in "
               <> rn (dim v)
               <> text ":\n"
               <> printVector 2 (map toDouble (components v))
\end{code}

%------------------------------------------------------------------------------%

The \module{Vector} module provides the \code{vector} functions for the smart
 construction of vectors. Even though right now the constructor does nothing
 except constructing the vector, the constructor is provided to hide the
 internal implementation to other modules.

%------------------------------------------------------------------------------%

\begin{code}

-- | Create vector from list of components.
vector :: [a] -> Vector a
vector = Vector

\end{code}

%------------------------------------------------------------------------------%
 Vectors can be manipulated using the \code{apply} function, that applies the
 given function to each element of the vector. For more complex computations,
 the components can be retrieved as a list using \code{toList}.
%------------------------------------------------------------------------------%

\begin{code}

-- | Apply the given functions to the elements of the vector.
apply :: Vector a -> (a -> a) -> Vector a
apply (Vector l) f = Vector (map f l)

-- -- | Return vector components as list.
-- toList :: Vector a -> [Double]
-- toList (Vector l) = l

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

-- -- | The dot product of two vectors.
-- dot :: Vector -> Vector -> Double
-- dot (Vector l1) (Vector l2) = foldl (\s (x,y) -> s + x*y) 0  $ zip l1 l2

\end{code}

%------------------------------------------------------------------------------%

The function \code{unitVector} creates the unit vector along the $i$th dimension
 in $n$-dimensional euclidean space.

%------------------------------------------------------------------------------%

\begin{code}

-- -- | ith unit vector in R^n
-- unitVector' :: Num a
--            => Int -- n
--            -> Int -- i
--            -> Vector a
-- unitVector' n i
--     | (n > 0) && (i >= 0) && (i < n) = Vector $ concat [replicate i 0,
--                                                         [1],
--                                                         replicate (n-i-1) 0]
--     | otherwise = error "unitVector: invalid input!"

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
-- pow :: Num a => Vector a -> MI.MultiIndex -> Double
-- pow (Vector cs) = pow' cs . MI.toList

-- pow' [] [] = mulId
-- pow' (v:vs) (i:is) = v ** fromIntegral i * pow' vs is
-- pow' _ _ = error "powV: Lists do not have equal length"
\end{code}
