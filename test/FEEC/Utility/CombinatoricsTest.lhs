
\section{Combinatorics}

This module defines properties for the functions in the \module{Combinatorics}
module to be used with the \code{QuickCheck}\cite{package:qc} module.

%------------------------------------------------------------------------------%

\ignore{
\begin{code}
{-# LANGUAGE TemplateHaskell #-}

module CombinatoricsTest (
                          test_combinatorics
                          ) where

import FEEC.Utility.Combinatorics ( increasingLists,
                                    increasingLists1,
                                    unrank,
                                    sublists,
                                    sumRLists,
                                    sumRLists' )
import qualified FEEC.Utility.Combinatorics as C ( choose )
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List (sort, (\\))

\end{code}
}

%------------------------------------------------------------------------------%

To test all properties in the module use \code{test_combinatorics}.

%------------------------------------------------------------------------------%

\begin{code}

return []
test_combinatorics = $quickCheckAll

\end{code}

%------------------------------------------------------------------------------%

\subsection{Increasing Lists}

In order to perform randomized test on the functions for the generation and
 indexing of increasing lists, it is necessary to generate parameter pairs $k,n$
, where $k$ is the lengths of the list and $n$ defines the set $\{0,\ldots,n\}$
or $\{1,\ldots,n\}$ from which the elements of the list are drawn. To this end
we defined the \code{Parameters} type and use it to generate $k,n$ pairs with
$k \leq n$ and $n \leq 100$.

%------------------------------------------------------------------------------%

\begin{code}
-- | Data type for parameters k,n defining an increasing list.
data Parameters = Parameters Int Int deriving (Eq, Show)

-- | Arbitrary instance that randomly chooses k,n such that 0 <= k <= n <= 20.
instance Arbitrary Parameters where
    arbitrary = do
      n <- choose (0,20)
      k <- choose (0,n)
      return (Parameters k n)
\end{code}

%------------------------------------------------------------------------------%

The defining property of increasing lists is of course that they are increasing.
So one obvious property to test for is of course that the lists returend by
\code{increasingLists} and \code{increasingLists'} are indeed
increasing.

%------------------------------------------------------------------------------%

\begin{code}

-- | Test if the given list is increasing.
increasing :: Ord a => [a] -> Bool
increasing l = and $ zipWith (<) l (tail l)

-- | Lists returned by 'increasingLists', 'increasingLists'' and 'unrank' should be
-- | increasing.
prop_increasing :: Parameters -> Bool
prop_increasing (Parameters k n) = (increasing (increasingLists k n))
                                  && (increasing (increasingLists1 k n))

\end{code}

%------------------------------------------------------------------------------%

As noted in \ref{sec:Combinatorics}, for given $k,n$ the number of increasing
lists is $(n + 1) \choose k$ or $n \choose k$, depending on whether the zero is
included in the list. The \code{increasingLists} and the \code{increasingLists1}
 functions should therefore return exactly this number of lists.

%------------------------------------------------------------------------------%

\begin{code}

-- | The number of lists returned by increasingLists must be the number of all
-- | increasing lists with length k + 1 and elements in [0..n].
prop_number :: Parameters -> Bool
prop_number (Parameters k n) =
    length (increasingLists k n) == (n + 1) `C.choose` k

-- | The number of lists returned by increasingLists must be the number of all
-- | increasing lists with length k + 1 and elements in [1..n].
prop_number1 :: Parameters -> Bool
prop_number1 (Parameters k n) =
    length (increasingLists1 k n) == n `C.choose` k
\end{code}

%------------------------------------------------------------------------------%

We also require the lists returned by \code{increasingLists} and
\code{increasingLists1} to be in lexicographic order.

%------------------------------------------------------------------------------%

\begin{code}

-- | The lists returned by increasingLists should be in order.
prop_ordered :: Parameters -> Bool
prop_ordered (Parameters k n) = and $ zipWith (<) l (tail l)
    where l = increasingLists k n

-- | The lists returned by increasingLists1 should be in order.
prop_ordered1 :: Parameters -> Bool
prop_ordered1 (Parameters k n) = and $ zipWith (<) l (tail l)
    where l = increasingLists1 k n

\end{code}

%------------------------------------------------------------------------------%

The \code{unrank} function must of course return the $i$th ordered increasing
list. Eventhough this property is somewhat trivial since this is just how the
function is implemented, we include it here for the case that the implementation
shoul be chaged.

%------------------------------------------------------------------------------%

\begin{code}

-- | For given index i 'unrank' should return the ith element of the increasing
-- | lists in lexicographic order.
prop_index :: Parameters -> Int -> Bool
prop_index (Parameters k n) i =
    (increasingLists k n) !! i' == unrank k n i'
        where i' = i `mod` ((n + 1) `C.choose` k)

\end{code}

%------------------------------------------------------------------------------%

In order to test the \code{sublists} function, we generate an increasing list
 using a \code{Parameters} instance and compute its sublists. Since this is also
 how this function is used in \code{FEEC}, this should be a reasonable
simplification.

The sublists of length $k-1$ should all have length $k-1$ and preserve the
ordering of the original list. Moreover for a length $k$ list, there should be
$k$ such sublists.

%------------------------------------------------------------------------------%

\begin{code}
-- | For a given list of length k, 'sublists' should return k lists.
prop_sublists_number :: Parameters -> Int -> Bool
prop_sublists_number (Parameters k n) i = length(sublists l) == k
    where l = unrank k n i'
          i' = i `mod` ((n + 1) `C.choose` k)

-- | 'sublists' should preserve the order of the input list.
prop_sublists_order :: Parameters -> Int -> Bool
prop_sublists_order (Parameters k n) i = all increasing (sublists l)
    where l = unrank k n i'
          i' = i `mod` ((n + 1) `C.choose` k)

-- | Lists returend by 'sublists' for a length k list should have length k - n1.
prop_sublists_length :: Parameters -> Int -> Bool
prop_sublists_length (Parameters k n) i = all ((k - 1)==) (map length (sublists l))
    where l = unrank k n i'
          i' = i `mod` ((n + 1) `C.choose` k)
\end{code}

%------------------------------------------------------------------------------%

\subsection{Lists of Degree $r$}

To simplify the testing of the functions that generate lists of a certain degree
, we introduce a data type for small positive integers.

%------------------------------------------------------------------------------%

\begin{code}
-- | Data type to represent small integers that are suitable for lengths of
-- | lists.
data SmallInt = SmallInt Int
                deriving (Eq, Show)

-- | Randomly choose an integer from the set [0,..,20].
instance Arbitrary SmallInt where
    arbitrary = do n <- choose (0, 10)
                   return (SmallInt n)
\end{code}

%------------------------------------------------------------------------------%

The lists of degree $r$ generated by \code{sumRLists} should contain only
positive integers which sum to $r$ and are of the given length. Similarly,
\code{sumRLists'} should return lists that sum to $r$ or less.

%------------------------------------------------------------------------------%

\begin{code}
-- | Lists returned by 'prop_sum_r' should sum to r.
prop_sum_r :: SmallInt -> SmallInt -> Bool
prop_sum_r (SmallInt n) (SmallInt r) = all (r ==) $ map sum (sumRLists n r)

-- | Lists returned by 'prop_sum_r' should have length n.
prop_sum_r_length :: SmallInt -> SmallInt -> Bool
prop_sum_r_length (SmallInt n) (SmallInt r) = all (n ==) $ map length (sumRLists n r)

-- | Lists returend by 'prop_sum_r_positive' should contain only elements
-- | larger or equal zero.
prop_sum_r_positive :: SmallInt -> SmallInt -> Bool
prop_sum_r_positive (SmallInt n) (SmallInt r) = and $ map (all (0<=)) (sumRLists n r)
\end{code}

The same properties should of course hold for \code{sumRLists'} with the
equality in the first property relaxed to less or equal.

\begin{code}
-- | Lists returned by 'prop_sum_r' should sum to r.
prop_sum_r' :: SmallInt -> SmallInt -> Bool
prop_sum_r' (SmallInt n) (SmallInt r) = all (r >=) $ map sum (sumRLists' n r)
    where l = sumRLists' n r

-- | Lists returned by 'prop_sum_r' should have length n.
prop_sum_r'_length :: SmallInt -> SmallInt -> Bool
prop_sum_r'_length (SmallInt n) (SmallInt r) = all (n ==) $ map length l
    where l = sumRLists' n r

-- | Lists returend by 'prop_sum_r_positive' should contain only elements
-- | larger or equal zero.
prop_sum_r'_positive :: SmallInt -> SmallInt -> Bool
prop_sum_r'_positive (SmallInt n) (SmallInt r) = and $ map (all (0<=)) l
    where l = sumRLists' n r

\end{code}


main = quickCheck prop_ordered

