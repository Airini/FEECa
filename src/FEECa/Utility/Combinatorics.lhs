
\section{Combinatorics}

The \code{Combinatorics} module contains functions for  common combinatorial
tasks that arise in \code{FEECa}. Those are
\begin{itemize}
  \item computation of binomial coefficients and factorials,
  \item generation and indexing of increasing lists,
  \item generation of multi-indices of a given degree.
\end{itemize}

%------------------------------------------------------------------------------%

\begin{code}

module FEECa.Utility.Combinatorics (

  -- * Mathematical Functions
    choose, factorial

  -- * Increasing Lists
  -- ** Generation
  , increasingLists, increasingLists1
  -- ** Indexing
  , unrank

  -- * Lists of degree r
  , sublists, kSublists, sumRLists, sumRLists'

  ) where


import qualified Math.Combinatorics.Exact.Binomial  as B
import qualified Math.Combinatorics.Exact.Factorial as F

\end{code}

%------------------------------------------------------------------------------%

\subsection{Mathematical Functions}

The \code{Combinatorics} module re-exports the function \code{choose}
for the computation of binomial coefficients and the function
\code{factorial} for the computation of factorials from the
\textbf{exact-combinatorics} \cite{package:ec} package.

%------------------------------------------------------------------------------%

\begin{code}
-- | Simple wrapper for 'factorial' that returns Num types.
factorial :: Num a => Int -> a
factorial = fromInteger . F.factorial

-- | Simple wrapper for 'choose'
choose :: (Integral a, Num b) => a -> a -> b
choose i j = fromIntegral (B.choose i j)

\end{code}

%------------------------------------------------------------------------------%

\subsection{Increasing Lists}

Increasing lists of length $k$ with elements in $\{0,\ldots,n\}$ provide a
natural way of representing faces of simplices (c.f. \ref{sec:simplex}) and
 alternating $n$-forms (c.f. \ref{sec:forms}). An ordering on those lists
can be defined using lexicographic order. In lexicographic ordering, the
natural ordering of the intergers is extended to lists by the principle of
alphabetic ordering. Lexicographic ordering is also what Haskell uses to compare
lists. Take as an example $k=3, n=3$:

\begin{align}
  0,1,2 \\
  0,1,3 \\
  0,2,3 \\
  1,2,3
\end{align}

\subsubsection{Generation}

The main task of generating increasing lists is performed by
\code{increasingLists'}, which generates a list of all increasing lists beginning
 with a given integer number. value. \code{increasingLists} and
 \code{increasingLists1} provide wrappers for \code{increasingLists'} to
 generate the length-$k$ lists with elements in $\{0,\ldots,n\}$ and
 $\{1,\ldots,n\}$, respectively.

%------------------------------------------------------------------------------%

\begin{code}
-- | List all increasing lists of length k with elements in [0..n]
increasingLists :: Integral a
                => a -- k
                -> a -- n
                -> [[a]]
increasingLists k n = increasingLists' k n 0

-- | List all increasing lists of length k with elements in [1..n]
increasingLists1 :: Integral a
                => a -- k
                -> a -- n
                -> [[a]]
increasingLists1 k n = increasingLists' k n 1

-- | List all increasing lists of length k with elements in [0..n] starting
-- | with x0.
increasingLists' :: Integral a
                 => a -- k
                 -> a -- n
                 -> a -- x0
                 -> [[a]]
increasingLists' k n x0
    | k < 1     = [[]]
    | k == 1    = [[x] | x <- [x0 .. n]]
    | otherwise = [ x : xs | x <- [x0 .. (n - k) + 1],
                             xs <- increasingLists' (k - 1) n (x + 1)]
\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Indexing}

Increasing lists can be indexed by assigning index $0$ to the smallest list (in
lexicographic order) and then counting upward. The function \code{unrank}
returns the increasing list with index $i$ for given paramters $k,n$.

%------------------------------------------------------------------------------%

\begin{code}
-- | Compute the increasing list of length k with elements in [0..n]
-- | corresponding to a given index i and length k.
unrank :: Integral a
       => a -- k
       -> a -- n
       -> a -- the index i
       -> [a]
unrank k n i = increasingLists k n !! fromIntegral i

{- TODO: not used? Check when it might be a better idea (if at any time)
rank :: Int -> [Int] -> Int
rank = rank' 0

rank' :: Int -> Int -> [Int] -> Int
rank' _     _     []      = 0
rank' lower upper (l:ls)  = (l - lower) * (upper - l - 1) `choose` (k - 1)
                            + rank' (l + 1) upper ls
  where k = length ls + 1
-}

\end{code}

%------------------------------------------------------------------------------%

%% -- | Ranks of the k-1 increasing sublists of the k-increasing list given by its rank n
%% sublists :: Int -> Int -> [Int]
%% sublists 1 _ = [0]
%% sublists k n = sublists' [] 0 k n

%% sublists' :: [Int] -> Int -> Int -> Int -> [Int]
%% sublists' l _ 0 _ = l
%% sublists' ls r k n = sublists' (n':ls) (r + ccc) (k-1) (n - cc)
%%     where c = fromJust (find (\x -> n < (x `choose` k)) [k..])
%%           cc = (c-1) `choose` k
%%           ccc = (c-1) `choose` (k-1)
%%           n' = r + n - cc

The function \code{sublists} returns all length $k-1$ sublists of a given length
$k$ list. The ordering of the list is preserved.

%------------------------------------------------------------------------------%

\begin{code}

-- | List all length k-1 sublists of the given length k list.
sublists :: [a] -- the length k list
         -> [[a]]
sublists ls = [ take i ls ++ drop (i+1) ls | i <- [0 .. length ls - 1]]

kSublists :: Int -> [a] -> [[a]]
kSublists _ []     = [[]]
kSublists k ls@(h:t)
  | k <= 0         = [[]]
  | k >= length ls = [ls]
  | k == 1         = [[l] | l <- ls]
  | otherwise      = [h : ts | ts <- kSublists (k - 1) t] ++ kSublists k t

\end{code}

%------------------------------------------------------------------------------%

\subsection{Lists of Degree $r$}

For generating multi-indices of degree $r$, it is necessary to
generate lists of positive integers that sum to a given value. Note that here we
will use the parameter $n$ to denote the length of the list. This is because
those functions are used for the generation of multi-indices and
therefore in most cases have length equal to the dimension $n$ of the underlying
vector space.

Two variants of functions generating such lists are provided. The function
\code{sumRLists} returns a list of all length $n$ lists that sum to $r$. The
function \code{sumRLists'} lists all length $n$ lists that sum to $r$
\textbf{or less}.

%------------------------------------------------------------------------------%

\begin{code}

-- | All length n lists of integers whose entries sum to r.
sumRLists :: Integral a
          => a -- n
          -> a -- r
          -> [[a]]
sumRLists n r
    | r == 0    = [replicate (fromIntegral n) 0]
    | n == 1    = [[r]]
    | n < 1     = []
    | otherwise = concat [[ls ++ [l] | ls <- sumRLists (n-1) (r-l)] | l <- [0..r]]

-- | All length n lists of integers whose sum to r or less.
sumRLists' :: Integral a
           => a -- n
           -> a -- r
           -> [[a]]
sumRLists' n r = concat [ sumRLists n r' | r' <- [0..r]]

\end{code}

\section{Bibliography}
