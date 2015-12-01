\section{Finite Element Spaces}

The \code{FiniteElementSpace} module provides functions for the generation of
finite element bases over simplices. The implementation is based on the article
 by Arnold, Falk and Winther \cite{ArnoldFalkWinther}.

Bases are provided for the two families of spaces $\pl{r}{k}$ and $\pml{r}{k}$.
 $\pl{r}{k}$ is the space of polynomial differential $k$-forms of degree at most
 $r$. The space $\pml{r}{k}$ is defined as

\begin{align}
\pml{r}{k} &= \{ \omega \in \pl{r}{k} | \kappa \omega \in \pl{r}{k-1} \}
\end{align}

where $\kappa$ is the Koszul operator.

 %------------------------------------------------------------------------------%

\begin{code}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module FEEC.FiniteElementSpace (
                                FiniteElementSpace(..),
                                BasisFunction,
                                Simplex,
                                Vector,
                                basis,
                                vspaceDim,
                                whitneyForm
 -- * Introduction
 -- $intro
)where

import Data.List
import FEEC.Bernstein (monomial, multiIndices)
import qualified FEEC.Bernstein as B (BernsteinPolynomial(..),extend)
import FEEC.Utility.Combinatorics
import qualified FEEC.PolynomialDifferentialForms as D
import FEEC.Utility.Print( printBernstein, printForm, dlambda )
import qualified FEEC.Utility.Print as P ( Pretty(..) )
import FEEC.Internal.Form hiding (arity, inner)
import qualified FEEC.Internal.Form as F(inner)
import qualified FEEC.Internal.Simplex as S
import FEEC.Internal.Spaces hiding (inner)
import qualified FEEC.Internal.Spaces as S(inner)
import qualified FEEC.Internal.Vector as V
import qualified FEEC.Internal.MultiIndex as MI
import qualified Math.Combinatorics.Exact.Binomial as CBin

-- $intro
-- This file implements the finite element space $P_r\Lambda^k$ and $P_r^-\Lambda^k$
--  in n dimensions. The implementation is based on the article "Geomentric De-
-- compositions and Local Bases for Spaces of Finite Element Differential Forms" by
-- Arnold, Falk, Winther. References given in the comments refer to this article.

\end{code}

%------------------------------------------------------------------------------%

\subsection{The \code{FiniteElementSpace} Data Type}

For the handling of finite element spaces the \code{FiniteElementSpace} module
provides the \code{FiniteElementSpace} type which represents a finite element
space over a simplex. It provides three type constructors. The \code{PrLk} and
the \code{PrmLk} constructors to represent the spaces $\pl{r}{k}$ and
$\pml{r}{k}$, respectively and the \code{GenSpace} constructor to represent a
general space by a set of basis functions.

%------------------------------------------------------------------------------%

\begin{code}
type Vector = V.Vector Double
type Simplex = S.Simplex Vector
type BernsteinPolynomial = D.BernsteinPolynomial Double
type DifferentialForm = D.DifferentialForm Double

-- | Data type for the two families of finite element spaces $P_r\Lambda^k$ and
-- | $P_r^-\Lambda^k$ defined over a simplex.
data FiniteElementSpace = PrLk Int Int Simplex
                        | PrmLk Int Int Simplex
                        | GenSpace [BasisFunction]
                          deriving( Show )

-- | Type synomym for basis function of finite element spaces that are of type
-- | Form BernsteinPolynomial.
type BasisFunction = DifferentialForm

\end{code}

%------------------------------------------------------------------------------%

For the $\pl{r}{k}$ and $\pml{r}{k}$ spaces the following functions can be used
to extract their properties. \code{degree} returns the maximum degree $r$ of the
spaces, while \code{arity} returns the arity $k$ of the alternating forms.
\code{vspaceDim} returns the dimension $n$ of the underlying euclidean space
$\R{n}$.

%------------------------------------------------------------------------------%

\begin{code}

-- | The degree of the finite element space.
degree :: FiniteElementSpace -> Int
degree (PrLk r _ _) = r
degree (PrmLk r _ _) = r

-- | The arity of the finite element space
arity :: FiniteElementSpace -> Int
arity (PrLk _ k _) = k
arity (PrmLk _ k _) = k

-- | The dimension of the underlying vector space.
vspaceDim :: FiniteElementSpace -> Int
vspaceDim (PrLk _ _ t) = S.geometricalDimension t
vspaceDim (PrmLk _ _ t) = S.geometricalDimension t

\end{code}

%------------------------------------------------------------------------------%

The function \code{dim} can be used to compute the dimension of the spaces which
are given by

\begin{align}
      \text{dim }\pml{r}{k} &= \ch{n + r}{n}\ch{n}{k} \\
      \text{dim }\pml{r}{k} &= \ch{r + k - 1}{k}\ch{ n + r }{ n - k }
\end{align}

%------------------------------------------------------------------------------%

\begin{code}


-- | The dimension of the finite element space.
instance Dimensioned FiniteElementSpace where
    dim (PrmLk r k t) = ((r + k - 1) `CBin.choose` k) * ((n + k) `CBin.choose` (n - k))
        where n = S.geometricalDimension t
    dim (PrLk r k t) = ((r + k) `CBin.choose` r ) * ((n + r) `CBin.choose` (n - k))
        where n = S.geometricalDimension t

\end{code}

%------------------------------------------------------------------------------%

Finally, the actual work is performed by the \code{basis} function which computes
the basis elements of a given $\pl{r}{k}$ or $\pml{r}{k}$ space. The implementation
is described below.

%------------------------------------------------------------------------------%

\begin{code}

-- | List the basis functions of the given finite element space.
basis :: FiniteElementSpace -> [BasisFunction]
basis (PrmLk r k t) = prmLkBasis r k t
basis (PrLk r k t) = prLkBasis r k t

pPrintBasisFunction :: BasisFunction -> String
pPrintBasisFunction = show . printForm dlambda "0" P.pPrint
                           . constituents

pPrint :: [BasisFunction] -> IO ()
pPrint [] = putStrLn "Empty List\n"
pPrint l  = putStrLn $ "[ " ++ foldl render (pPrintBasisFunction (head l)) (tail l)
                            ++ "]"
    where render s bf = s ++ ",\n" ++ pPrintBasisFunction bf

\end{code}

%------------------------------------------------------------------------------%

\subsection{Concrete Spaces}

For the construction of finite element spaces over a given simplex the
\code{finiteElementSpace} function is provided. It takes the name of one
of the primary spaces of finite elements as presented in \cite{ArnoldLogg}.

%------------------------------------------------------------------------------%

\begin{code}
-- | Name data type to represent the named finite elements. See
-- | <http://www.femtable.org Periodic Table of the Finite Elements>.
data Name = P
          | Pm
          | DP
          | DPm
          | RT
          | N1e
          | N1f
          | BDM
          | N2e
          | N2f

-- | Create named finite element space of given polynomial degree over the
-- | given simplex.
finiteElementSpace :: Name -> Int -> Simplex -> FiniteElementSpace
finiteElementSpace P r t = PrLk r 0 t
finiteElementSpace Pm r t = PrmLk r 0 t
finiteElementSpace DP r t = PrLk r (S.topologicalDimension t) t
finiteElementSpace DPm r t = PrmLk r (S.topologicalDimension t) t
finiteElementSpace RT r t
    | S.topologicalDimension t == 2 = PrmLk r 1 t
    | otherwise = error "RT elements are defined in two dimensions."
finiteElementSpace N1e r t
    | S.topologicalDimension t == 3 = PrmLk r 1 t
    | otherwise = error "N1e1 elements are defined in three dimensions."
finiteElementSpace N1f r t
    | S.topologicalDimension t == 3 = PrmLk r 2 t
    | otherwise = error "N1f1 elements are defined in three dimensions."
finiteElementSpace BDM r t
    | S.topologicalDimension t == 2 = PrLk r 1 t
    | otherwise = error "BDM elements are defined in two dimensions."
finiteElementSpace N2e r t
    | S.topologicalDimension t == 3 = PrLk r 1 t
    | otherwise = error "N2e1 elements are defined in three dimensions."
finiteElementSpace N2f r t
    | S.topologicalDimension t == 3 = PrLk r 2 t
    | otherwise = error "N2f1 elements are defined in three dimensions."

\end{code}

%------------------------------------------------------------------------------%
\subsection{Construction of Bases}

For the construction of finite element bases we use the geometric decompositions
 proposed in section 9 in \cite{ArnoldFalkWinther}.

Let $\smp{f}$ be a face of a simplex $\smp{T}$ In the following we write
$\mathcal{I}(\smp{f})$ to denote the set of indices of vertices of $\smp{T}$
included in the subsimplex $\smp{f}$. This is the same as the range of the map
$\sigma$ representing the subsimplex

\begin{align}
  \mathcal{I}(\smp{f}) &= \text{range} ( \sigma )
\end{align}

Moreover, we define the range operator applied to a face and a map $\sigma$:

\begin{align}
  \text{range}(\smp{f},\sigma) = \mathcal{I}(\smp{f}) \cup \text{range}(\sigma)
\end{align}

%------------------------------------------------------------------------------%

\begin{code}

-- | The range operator for a multi-index and an increasing list.
range :: MI.MultiIndex -> [Int] -> [Int]
range mi sigma = sort (union sigma (MI.range mi))

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{The $\pml{r}{k}$ Spaces}

An import building block in the construction of bases for the $\pml{r}{k}$ spaces
are the so called Whitney forms. Let $\smp{f}$ be a subsimplex of $\smp{T}$ and
 $\sigma \in \Sigma(0:k,0:n)$. The Whitney form $\phi_\sigma^\smp{f}$ is defined
as:

\begin{align}
  \phi_\sigma^{\smp{f}} &= \sum_{i=0}^k (-1)^i\lambda^\smp{f}_i d\lambda^{\smp{f}}_0
                          \wedge \ldots \wedge d\lambda^{\smp{f}}_{i-1} \wedge
                          d\lambda^{\smp{f}}_{i+1} \wedge \ldots \wedge
                                          d\lambda^{\smp{f}}_k
\end{align}

where we have used $\lambda_i^{\smp{f}}$ to denote the barycentric coordinates
of the face $\smp{f}$.

%------------------------------------------------------------------------------%
\begin{code}

-- | The Whitney forms as given by  equation (6.3).
whitneyForm :: Simplex -> [Int] -> Form BernsteinPolynomial
whitneyForm t ls = Form k n [( lambda' (ls !! i), subsets !! i) | i <- [0..k]]
    where k = length ls - 1
          n = S.topologicalDimension t
          subsets = sublists ls
          lambda' i = sclV ((-1)^i) (monomial t (MI.unit (n + 1) i))

\end{code}

%------------------------------------------------------------------------------%

 The Whitney form $\phi_\sigma = \phi^{\smp{T}}$ is obtained by extending the
 differential form to $T$. The extension is performed simply by identifying the
 barycentric coordinates $\lambda^{\smp{f}}_i$ with the corresponding ones on
 the supersimplex

%------------------------------------------------------------------------------%

\begin{code}

-- | Extend a differential Form defined on a face of a simplex to the full simplex.
-- | Implements the barycentric extension operator defined in eq. (7.1) in
-- | Arnold, Falk, Winther.
extend :: Simplex -> Form BernsteinPolynomial -> Form BernsteinPolynomial
extend t (Form k n' cs) = Form k n (extend' cs)
    where extend' = map (\ (x,y) -> (B.extend t x, extendFace n y))
          n = S.topologicalDimension t

-- | Extend the face of a subsimplex of a simplex of topological dimension n to the
-- | the simplex. That is, for a face given as an increasing list of vertices of the
-- | subsimplex, return the list of vertices of the simplex that corresponds to that
-- | face.
extendFace :: Int -> [Int] -> [Int]
extendFace n f = [ [0..n] !! (i-1) | i <- f ]

\end{code}

%------------------------------------------------------------------------------%

Using the Whitney forms, a basis for $\pml{r}{k}$ can be constructed as follows.
For given $r$,$k$ and a simplex $\smp{T}$, a basis of the space $\pml{r}{k}$ is
given by the set

$$
\begin{align}\label{eq:basis_pl}
  \{(\lambda)^{\vec{\alpha}} \phi_\sigma | \: & \vec{\alpha} \in \mathrm{N}_0^{0:n},
   |\vec{\alpha}| = r-1, \\
  & \sigma \in \Sigma(0:k,0:n), \text{range}(\vec{\alpha},\sigma) = \mathcal{I}(f), \\
  & \alpha_i = 0 \text{ if } i < \text{min}\left ( \text{range}(\sigma) \right ) \}
\end{align}
$$

The construction of this set is implemented in the function \code{prmLkBasis}.

%------------------------------------------------------------------------------%

\begin{code}

-- | The basis of the $P_r^-\Lambda^k$ space over the given simplex constructed
-- | using the geometric composition given by Anrold, Falk, Winther.
prmLkBasis :: Int -> Int -> Simplex -> [Form BernsteinPolynomial]
prmLkBasis r k t = concat [ map (extend t) (prmLkFace r k t')
                                | t' <- S.subsimplices' t k ]

-- | Basis for the space PrMinusLambdak with vanishing trace associated to
-- | a given face of a simplex.
prmLkFace :: Int -> Int ->Simplex -> [Form BernsteinPolynomial]
prmLkFace r k t = [sclV (b alpha) (whitneyForm t sigma) | alpha <- alphas,
                                                          sigma <- sigmas alpha]
    where n = S.topologicalDimension t
          b = monomial t
          alphas = MI.degreeR n (r-1)
          sigmas alpha = [ sigma | sigma <- increasingLists n k,
                                   range alpha sigma == [0..n],
                                   zero alpha sigma ]
          zero alpha sigma = all (0==) (take (minimum sigma) (MI.toList alpha))
\end{code}

%------------------------------------------------------------------------------%

\subsubsection{The $\pl{r}{k}$ Spaces}

The construction of the $\pl{r}{k}$ spaces uses another set primitive $k$-forms.
Let $\smp{f}$ be a face of another face $\smp{g}$ of the simplex $\smp{T}$.
Define $\psf{\alpha}{f}{g}{\sigma}$

\begin{align}
      \psf{\alpha}{f}{g}{i} &= d\lambda_i^{\smp{g}} -
          \frac{\alpha_i}{|\alpha|} \sum_{j \in \mathcal{I}(\smp{f})} d\lambda_j^\smp{g} \\
     \psf{\alpha}{f}{g}{\sigma} &= \psf{\alpha}{f}{g}{\sigma{(1)}} \wedge \ldots
                          \wedge \psf{\alpha}{f}{g}{\sigma{(k)}}
\end{align}

%------------------------------------------------------------------------------%

\begin{code}
-- | The psi forms that implement the extension operator for the $P_r\Lambda^k$
-- | spaces as given in equations (8.1) and (8.2) in Arnold, Falk, Winther.
psi :: MI.MultiIndex -> [Int] -> Form BernsteinPolynomial
psi alpha sigma  = foldl (/\) unit [ psi' alpha i | i <- sigma]
    where unit = nullForm (n+1) mulId
          n = dim alpha - 1

-- TODO: Check form indices.
psi' :: MI.MultiIndex -> Int -> Form BernsteinPolynomial
psi' alpha i = foldl subV (db i) [ sclV (c j) (db j) | j <- [0..n]]
    where db j = oneForm (j+1) (n+1) -- Dimension should be n ?
          c j = sclV (fromIntegral (alpha' !! i) / fromIntegral r) mulId
          r = MI.degree alpha :: Int
          alpha' = MI.toList alpha :: [Int]
          n = dim alpha - 1
\end{code}

%------------------------------------------------------------------------------%

where $\sigma \in \Sigma(0:k,0:n)$ and $k$ and $n$ the topological dimensions of
$\smp{f}$ and $\smp{g}$, respectively.

A basis for the space $\pl{r}{k}$ is then given by the set

$$
\begin{align}\label{eq:basis_pml}
  \{(\lambda^{\smp{T}})^{\vec{\alpha}} \psi_\sigma | \: & \vec{\alpha} \in \mathrm{N}_0^{0:n},
   |\vec{\alpha}| = r, \\
  & \sigma \in \Sigma(1:k,0:n), \text{range}(\vec{\alpha},\sigma) = \mathcal{I}(f), \\
  & \alpha_i = 0 \text{ if } i < \text{min}\left (\mathcal{I}(\smp{f}) /\ \text{range}(\sigma) \right ) \}
\end{align}
$$

%------------------------------------------------------------------------------%

\begin{code}

-- | Basis of the $P_r\Lambda^k$ space over the given simplex constructed using
-- | the geometric decomposition given by Arnold, Falk, Winther.
prLkBasis :: Int -> Int -> Simplex -> [Form BernsteinPolynomial]
prLkBasis r k t = concat [ map (extend t) (prLkFace r k t') | t' <- S.subsimplices' t k ]

-- | Basis functions  for the space PrMinusLambdak associated to
-- | a given face of a simplex.
prLkFace :: Int -> Int -> Simplex -> [Form BernsteinPolynomial]
prLkFace r k t = [ sclV b (psi' (alpha b) sigma) | (b, sigma) <- fs ]
    where fs = map (head . constituents) (prLkFace' r k t)
          psi' alpha sigma = extend t (psi alpha sigma)
          alpha b = multiIndices b !! 0
          n = S.topologicalDimension t

-- | Basis for the space PrMinusLambdak with vanishing trace associated to
-- | a given face of a simplex.
prLkFace' :: Int -> Int -> Simplex -> [Form BernsteinPolynomial]
prLkFace' r k t = [Form k n [(b alpha, sigma)] | alpha <- alphas,
                                                   sigma <- sigmas alpha]
    where n = S.topologicalDimension t
          b = monomial t
          alphas = MI.degreeR (n + 1)  r
          sigmas alpha = [ sigma | sigma <- increasingLists k n,
                                   range alpha sigma == [0..n],
                                   zero alpha sigma ]
          zero alpha sigma = all (0==) (take (minimum' sigma) (MI.toList alpha))
          minimum' sigma = minimum ([0..n] \\ sigma)

t = S.referenceSimplex 3
t2 = S.subsimplex t 2 0
t1 = S.subsimplex t 1 0

b = monomial t1 (MI.multiIndex [1,2])
omega = Form 2 2 [(b, [0,1])]
eta = Form 0 2 [(b,[0])]

alpha = MI.multiIndex [2,1]
sigma = [0]
s1 = finiteElementSpace N2f 3 t



\end{code}

\section{Bibliography}
