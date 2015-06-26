%------------------------------------------------------------------------------%

\section{Multi-Indices}

The \module{MultiIndex} module provides a data type and functions for
multi-indices. A multi-index $\vec{\alpha}$ is a tuple

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

The support $\llbracket \vec{\alpha} \rrbracket$ of a multi-index $\vec{\alpha}$
is defined as

\begin{align}
  \llbracket \vec{\alpha} \rrbracket &= \{ i \in [0,n-1] | \alpha_i > 0 \}
\end{align}

, i.e. the set of indices $i$ for which the exponents $\alpha_i$ are non-zero.

\ignore{
\begin{code}

module FEEC.Internal.MultiIndex(

  -- * The MultiIndex type
  MultiIndex, toList,

  -- ** Constructors
  multiIndex, zero, one, degreeR,

  -- * Extension
  extend,

  -- * Mathematical Operations
  add, decrease, factorial, choose

  ) where


import Data.Traversable
import Data.Bits
import FEEC.Internal.Spaces(Dimensioned(..))
import FEEC.Utility.Combinatorics(sumRLists)
import qualified Math.Combinatorics.Exact.Binomial as CBin
import qualified Math.Combinatorics.Exact.Factorial as CFac
import Control.Applicative(Applicative(..), ZipList(..), liftA, liftA2)

\end{code}
}

%------------------------------------------------------------------------------%

\subsection{The \code{MultiIndex} type}

Multi-indices are implemented using the \code{ZipList} type because
the use of \code{Applicative} class methods simplifies the implementation of
many operations on multi-indices.

The dimension of a multi-index is the dimension of the underlying space, i.e.
the number of exponents in the multi-index.

%------------------------------------------------------------------------------%

\begin{code}

type MultiIndex = ZipList Int

instance Dimensioned (ZipList a) where
    dim mi = (length (getZipList mi)) - 1

-- | Transform multi-index into list
toList :: Integral a => MultiIndex -> [a]
toList = map fromIntegral . getZipList
\end{code}

%------------------------------------------------------------------------------%

The properties degree and range of mult-indices above, are implemented in a
straight-forward manner.

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
      | l /= 0 = i:(range' (i+1) (ZipList ls))
      | otherwise = range' (i+1) (ZipList ls)
range' _ (ZipList []) = []

\end{code}

%------------------------------------------------------------------------------%

\begin{code}


\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Constructors}

The most straight forward way of constructing a multi-index is from a list of
integers representing the exponents. This is done using the \code{multiIndex}
function.

%------------------------------------------------------------------------------%

\begin{code}

-- | Create a multi-index from a given list of integers.
multiIndex :: [Int] -> MultiIndex
multiIndex l
    | valid mi = mi
    | otherwise = error "multiIndex: Multi-index is invalid!"
    where mi = ZipList l

-- | Check whether a given multi-index is valid.
valid :: MultiIndex -> Bool
valid mi = all (0 <=) (toList mi)
\end{code}

%------------------------------------------------------------------------------%

A common use case is the creation of multi-indices of degree 0 or 1. This is
done using the functions \code{zero} and \code{one}. The function \code{zero}
creates a multi-index containing only zeros of given dimension. The function
\code{one} creates a multi-index of given dimension with all elements equal to
zero except for $i$th.

%------------------------------------------------------------------------------%

\begin{code}

-- | Degree zero multi-index
zero ::Int -> MultiIndex
zero n = ZipList (replicate n 0)

-- | Degree one multi-index of diemension n with i-th element equal to one and
-- | all others zero.
one :: Int -- n
    -> Int -- i
    -> MultiIndex
one n i = ZipList $ concat [replicate i 0,[1],replicate (n-i) 0]

\end{code}

%------------------------------------------------------------------------------%

The function \code{degreeR} returns a list of all multi-indices of given
dimension $n$ and degree $r$.

%------------------------------------------------------------------------------%

\begin{code}

-- | List of all dimension n (length n + 1) multi-indices of degree r
degreeR :: Integral a => a -> a -> [MultiIndex]
degreeR n r = map ZipList $ sumRLists (fromIntegral (n + 1)) (fromIntegral r)

\end{code}

%------------------------------------------------------------------------------%

\subsection{Extension of Multi-Indices}

For the extension of polynomials from subsimplices to simplices it is necessary
to also extend the multi-indices from a face to the full simplex. To this end we
assume that the face of dimension $k$ is represented by an increasing list
$\sigma=(\sigma_0,\ldots,\sigma_k)$ with elements in $[0,n]$. The $i$th element
in $\sigma$ associates the $i$th element in the multi-index $\vec{\alpha}_0$
with the $\sigma_i$th element in the extended multi-index $\vec{\alpha}$ on the
$n$-simplex:

\begin{align}
  (\vec{\alpha}_0)_i \rightarrow (\vec{\alpha}_0)_{\sigma_i}
\end{align}

All other elements $\alpha_i$ in $\vec{\alpha} that are not in \sigma are zero.
The extension of multi-indices to n-dimensional super-simplices is implemented
by the \code{extend} function.

%------------------------------------------------------------------------------%

\begin{code}

-- | Extend multi-index from from a face to a simplex.
extend :: Int -> [Int] -> MultiIndex -> MultiIndex
extend n sigma mi = multiIndex $ extend' n (-1) sigma mi'
    where mi'       = toList mi

extend' :: Int -> Int -> [Int] -> [Int] -> [Int]
extend' n i (s:ss) (j:js) = (replicate di 0) ++ (j : (extend' (n - di - 1) s ss js))
    where di = s - i - 1  -- Number of zeros to pad.
extend' n i [] [] = replicate n 0

\end{code}

%------------------------------------------------------------------------------%

\subsection{Mathematical Operations}

Addition on multi-indices is defined in a straight-foward manner as the addition
of the each pair of elements separately.

For the derivation of polynomials, it is required to decrease the element at a
given index of a multi-index, this is implemented by the \code{decrease}
function.

The factorial of a multi-index is defined as the product of the factorial of all
its elements:

\begin{align}
  \vec{\alpha}! &= \prod_{i=0}^{n-1} \alpha_i!
\end{align}

The binomial coefficient of two multi-index is defined as the product of the
element-wise binomial coefficients.

\begin{align}
  \vec{\alpha} \choose \vec{\beta} &= \prod_{i=0}^{n-1} \alpha_i \chose \beta_i
\end{align}

%------------------------------------------------------------------------------%

\begin{code}

-- | Add two multi-indices
add :: (Integral a) => ZipList a -> ZipList a -> ZipList a
add = liftA2 (+)

-- | Decrease element in multi-index
decrease :: Integral a =>  Int -> ZipList a -> ZipList a
decrease i alpha  = pure f <*> ZipList [0..] <*> alpha
    where f j a = if j == i then max 0 (a-1) else a

-- | Generalized binomial coefficients for multi-indices as defined in the paper
-- | by Kirby.
choose :: (Integral a) => ZipList a -> ZipList a -> a
choose a b = product $ getZipList $ liftA2 CBin.choose a b

-- | Generalized factorial for multi-indices
factorial :: (Bits a, Integral a) => MultiIndex -> a
factorial = product . map CFac.factorial . toList

\end{code}

