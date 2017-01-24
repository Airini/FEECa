%------------------------------------------------------------------------------%

\section{Multi-Indices}

The \code{MultiIndex} module provides a data type and functions for handling
of multi-indices. A multi-index $\vec{\alpha}$ is a tuple

\begin{align}
  \vec{\alpha} &= (\alpha_0,\ldots,\alpha_{n-1}), \quad \alpha_i \in \mathrm{N}_0^+
\end{align}

that generalized the notion of an exponent for vectors in $\R{n}$. The power of
 a vector $\vec{x}^{\vec{\alpha}}$ is defined as

\begin{align}
  \vec{x}^{\vec{\alpha}} &= \prod_{i = 0}^{n-1} x_i^{\alpha_i}
\end{align}
The degree of a multi-index $\vec{\alpha}$ is the sum of exponents in the tuple:

\begin{align}
  |\vec{\alpha}| &= \sum_{i=0}^{n-1} \alpha_i
\end{align}

 The support $\text{supp} \left \{ \vec{\alpha} \right \}$ of a multi-index
 $\vec{\alpha}$ is defined as the set of indices $i$ for which the exponents
 $\alpha_i$ are non-zero:

\begin{align}
 \text{supp}\left \{ \vec{\alpha} \right \}  &= \{ i \in [0,n-1] ~|~ \alpha_i > 0 \}
\end{align}

\begin{code}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module FEECa.Internal.MultiIndex(

  -- * The MultiIndex type
    MultiIndex, toList, valid

  -- ** Constructors
  , multiIndex, zero, unit, degreeR

  -- * Extension
  , extend

  -- * Mathematical Operations
  , add, decrease, derive, factorial, choose, choose', degree, range

  ) where


import            Control.Applicative               (ZipList(..), liftA2, pure, (<*>))

import            FEECa.Utility.Combinatorics       (sumRLists)
import qualified  FEECa.Utility.Combinatorics as C  (choose, factorial)
import            FEECa.Internal.Spaces             (Dimensioned(..), Field(..))

\end{code}


%------------------------------------------------------------------------------%

\subsection{The \code{MultiIndex} type}

The \code{MultiIndex} type is implemented as a type synomnym for
\code{ZipList Int} type because this allows the use of the \code{Applicative}
class for the implementation of methods on multi-indices.

The dimension of a multi-index is the dimension of the underlying space, i.e.
the number of exponents in the multi-index.
%------------------------------------------------------------------------------%
\begin{code}

type MultiIndex = ZipList Int

instance Dimensioned MultiIndex where
  dim mi = length (getZipList mi)

\end{code}

%------------------------------------------------------------------------------%

The functions \code{degree} and \code{range} compute the degree and range of a
given multi-index, as defined above.

%------------------------------------------------------------------------------%

\begin{code}

-- | Degree of a multi-index, i.e. the sum of all indices
degree :: Integral a => MultiIndex -> a
degree = fromIntegral . sum . getZipList

-- | List indices of the multi-index that are non-zero.
range :: MultiIndex -> [Int]
range = range' 0

range' :: Int -> MultiIndex -> [Int]
range' i (ZipList (l:ls))
      | l /= 0    = i : range' (i+1) (ZipList ls)
      | otherwise =     range' (i+1) (ZipList ls)
range' _ (ZipList []) = []

\end{code}

%------------------------------------------------------------------------------%
\subsubsection{Constructors}


 The \code{MultiIndex} type provides the \code{multiIndex} constructor which
 constructs a multi-index from a list of integers \code{[Int]}. The constructor
 makes sure that all entries of the multi-index are positive and throws a
 run-time error otherwise.

 The \code{toList} function transforms a given multi-index back to a list
 \code{[a]} of \code{Integer} class type.
%------------------------------------------------------------------------------%

\begin{code}
-- | Create a multi-index from a given list of integers.
multiIndex :: [Int] -> MultiIndex
multiIndex l
    | valid mi  = mi
    | otherwise = error "multiIndex: Multi-index is invalid!"
  where mi = ZipList l

-- | Check whether a given multi-index is valid.
valid :: MultiIndex -> Bool
valid mi = all (0 <=) (toList mi)

-- | Transform multi-index into list
toList :: Integral a => MultiIndex -> [a]
toList = map fromIntegral . getZipList

\end{code}

%------------------------------------------------------------------------------%

A common use case is the creation of multi-indices of degree 0 or 1.
%
This is done using the functions \code{zero} and \code{unit},
respectively.
%
The function \code{zero} creates a multi-index of given dimension
containing only zeros, while
%
\code{unit} creates a multi-index of given dimension with all elements
equal to zero except for the element with index $i$:

\begin{align}
  (a_0,\ldots,a_{n-1}) \text{ with }
  \alpha_0,\ldots,\alpha_{i-1},\alpha_{i+1},\ldots,\alpha_{n-1} = 0
  \text{ and } \alpha_i = 1
\end{align}

%------------------------------------------------------------------------------%

\begin{code}

-- | Degree zero multi-index
zero ::Int -> MultiIndex
zero n = ZipList (replicate n 0)

-- | Degree one multi-index of dimension n with i-th element equal to one and
-- | all others zero.
unit :: Int -- n
     -> Int -- i
     -> MultiIndex
unit n i = ZipList $ concat [replicate i 0,[1],replicate (n-i-1) 0]

\end{code}
%------------------------------------------------------------------------------%

The function \code{degreeR} returns a list of all multi-indices of
given dimension $n$ and degree $r$.
%
It is basically a wrapper for the \code{sumRLists} function provided
by the \code{FEECa.Utility.Combinatorics} module.

%------------------------------------------------------------------------------%


\begin{code}

-- | List of all dimension n (length n + 1) multi-indices of degree r
degreeR :: Integral a => a -> a -> [MultiIndex]
degreeR n r = map ZipList $ sumRLists (fromIntegral n) (fromIntegral r)

\end{code}

%------------------------------------------------------------------------------%
\subsection{Extension of Multi-Indices}
\label{sec:mi_extension}

For the extension of polynomials from a sub-simplex $\smp{f} =
[\vec{v_{i_0}},\ldots,\vec{v_{i_k}}]$ to a super-simplex $\smp{T} =
[\vec{v_{0}},\ldots,\vec{v_{m}}]$ it is necessary to also extend the
multi-indices from $\smp{f}$ to $\smp{T}$.
%
To this end we assume that the face of dimension $k$ is given by a
mapping $\sigma(j) = i_j$ that encodes which vertices of the
super-simplex $\smp{T}$ are included in $\smp{f}$.
%
Note that $m$ is not necessarily the dimension of the underlying
Euclidean space $\R{n}$.
%
Each element $\alpha_i$ in a multi-index $\vec{\alpha} =
(\alpha_0,\ldots,\alpha_k)$ defined over the face $\smp{f}$ represents
a power of the barycentric coordinate corresponding to the vertex
$\vec{v_i}$.
%
The extended multi-index $\vec{\alpha'}$ is then zero in all positions
corresponding to vertices of $\smp{T}$ that are not included in
$\smp{f}$ and coincides with $\vec{\alpha}$ on positions corresponding
to the same vertices.
%
The extended multi-index $\vec{\alpha'} =
({\alpha'_0,\ldots,\alpha'_m})$ is thus given by

\begin{align}
  \alpha'_j &= \begin{cases}
    \alpha_i & \text{, if } \sigma(i) = j \\
    0 & \text{otherwise}
    \end{cases}
\end{align}


%------------------------------------------------------------------------------%

\begin{code}

-- | Extend a multi-index from a face to a simplex.
extend :: Int -> [Int] -> MultiIndex -> MultiIndex
extend n sigma mi
    | length sigma == dim mi = multiIndex $ extend' n (-1) sigma mi'
    | otherwise = error $(show sigma) ++ " \n " ++ show mi ++ "extend: Dimensions of sigma and multi-index don't agree"
  where mi'       = toList mi

extend' :: Int -> Int -> [Int] -> [Int] -> [Int]
extend' n _ []      []      = replicate n 0
extend' n i (s:ss)  (j:js)  = replicate di 0 ++ (j : extend' (n - di - 1) s ss js)
  where di = s - i - 1  -- Number of zeros to pad.
extend' _ _ _       _       = error "extend': list argument lengths must match"
\end{code}

%------------------------------------------------------------------------------%

\subsection{Mathematical Operations}


\subsubsection{Addition}
Addition on multi-indices is defined in a straight-foward manner as
the addition of the each pair of elements separately.

%------------------------------------------------------------------------------%

\begin{code}

-- | Add two multi-indices
add :: (Integral a) => ZipList a -> ZipList a -> ZipList a
add = liftA2 (+)

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Factorial}

The factorial of a multi-index is defined as the product of the
factorial of all its elements:

\begin{align}
  \vec{\alpha}! &= \prod_{i=0}^{n-1} \alpha_i!
\end{align}

%------------------------------------------------------------------------------%

\begin{code}

-- | Generalized factorial for multi-indices
factorial :: Num b => MultiIndex -> b
factorial = product . map C.factorial . toList

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Binomial Coefficient}

The binomial coefficient of two multi-index is defined as the product
of the element-wise binomial coefficients.

\begin{align}
  \vec{\alpha} \choose \vec{\beta} &= \prod_{i=0}^{n-1} {\alpha_i \choose \beta_i}
\end{align}

%------------------------------------------------------------------------------%

\begin{code}
-- | Generalized binomial coefficients for multi-indices as defined in the paper
-- | by Kirby.
choose :: (Integral a, Num b) => ZipList a -> ZipList a -> b
choose a b = product (getZipList (liftA2 C.choose a b))

choose' :: Fractional b => Int -> ZipList Int -> b
choose' a b = fromIntegral (C.factorial a) / fromIntegral (factorial b)

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Derivation}

For the derivation of polynomials, it is required to decrease the
element at a given index of a multi-index, this is implemented by the
\code{decrease} function.

\begin{code}
-- | Decrease element in multi-index
decrease :: Integral a =>  Int -> ZipList a -> ZipList a
decrease i alpha  = pure f <*> ZipList [0..] <*> alpha
  where f j a = if j == i then max 0 (a-1) else a

-- | Decrease element in multi-index
derive :: (Integral a, Field b) =>  Int -> ZipList a -> (b, ZipList a)
derive i alpha  = (c, decrease i alpha)
  where c = (fromDouble . fromIntegral) (getZipList alpha !! i)
\end{code}
