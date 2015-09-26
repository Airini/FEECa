%------------------------------------------------------------------------------%
%
Consider a domain $\Omega$ in $n$-dimensional Euclidean space $\mathrm{R}^n$. In
finite element exterior calculus, we are dealing with tesselations of such domain
into finite elements.
%
In particular, we consider a triangulation of the domain $\Omega$ into a finite
number of $n$-dimensional simplices $\mathcal T = \{T_1,\ldots,T_n\}$.
%
This sections describes how those geometrical concepts are modeled in FEEC.

\subsection{Vectors}

%
Vectors in $\mathrm{R}^n$ are represented by the multi-parameter typeclass |EuclideanSpace v r|, where |v| represents the vector type and |r| the scalar type.
%
The superclass |VectorSpace| represents the arithmetic structure of a general vector space over a ring of scalars.
%
The |EuclideanSpace| class is basically an extension of the |VectorSpace| class that adds the inner product in $\mathrm{R}^n$ and the representation as finite lists of vector components to the funcitonality provided by the |VectorSpace| class.
%
\begin{code}
class (Eq v, Dimensioned v, VectorSpace v,
       Eq r, Field r, Scalar v ~ r)
       => EuclideanSpace v r where
    dot        :: v   -> v -> r
    fromList   :: [r] -> v
    toList     :: v   -> [r]
\end{code}
%
A concrete vector type is implemented by the parametrized |Vector a| type which represents vectors as a list |[a]| of type |a|.
%
The parameter type |a| allows to use different representation of the real numbers.
%
This allows us to choose between exact and double arithmetic, which can be useful for
 example for testing.
%
\begin{code}
data Vector a = Vector { components :: [a] }
               deriving (Show)
\end{code}

\subsection{Simplices}
%
A $k$-simplex $\smp{T} = [\vec{v_0},\ldots,\vec{v_k}]$ in $n$-dimensional Euclidean space $\R{n}$ is the convex hull of $k+1$ vertices $\vec{v_0},\ldots,\vec{v_k}$ such that the spanning vectors $\vec{v_1}-\vec{v_0} ,\ldots,\vec{v_k}-\vec{v_0}$ are linearly independent.
%
A subsimplex or face $f$ of dimension $k'$ is a simplex consisting of the convex hull of a subset of $k' + 1$ vertices of its supersimplex.
%
An important concept in finite element exterior calculus is the representation of faces of simplices using an increasing map $\sigma: \{0,\ldots,k'\} \mapsto \{0,\ldots,k\}$.
%
In the code, we represent such a map as an increasing list of type |[Int]|.
%
To represent general simplices, a list of vertices and such an increasing map is used
%
Keeping track of $\sigma$ for subsimplices is important to later extend polynomials from the face of a simplex to the full simplex.

\begin{code}
data Simplex a =  Simplex { sigma :: [Int],
                            vertices :: [a] }
                deriving (Eq, Show)
\end{code}

The barycentric coordinates $\lambda_0,\ldots,\lambda_k$ defined over a simplex $\smp{T} = \{\vec{v}_1,ldots,\vec{v}_k$ describe a point $\vec{x}$ on a simplex as a convex combination of the $k+1$ simplices:

\begin{align}
  \vec{x} = \lambda_0 \vec{v_0} + \ldots + \lambda_k \vec{v_k}
\end{align}

Viewed as a function of $vec{x}$, the barycentric polynomials form a basis for the space of affine functions on $\smp{T}$.
%
The barycentric coordinates obviously satisfy
\begin{align}
 \lambda_i(\vec{v}_j) = \delta_{ij}
\end{align}
where $\delta_{ij}$ is the Kronecker delta. This property results in a linear system of equations for the coefficients of the affine functions which can be solved to obtain the representation of the barycentric coordinates as an affine function in the components of $\vec{x}$.
%

An important operation in finite elemet exterior calculus is the computation of integrals over a finite element.
%
To compute the integral of an arbitrary function over a given simplex $T$, we use the technique described in \cite{Ainsworth}.
%
By a suitable coordinate transform, the integral of a function $f(\vec{x})$ over the simplex $T$ is turned into an the form

\begin{align}
  V(\smp{T}) \int_0^1 dt_1(1-t_1)^{n-1}\ldots\int_0^1 dt_nf(\vec{x}(t_0,\ldots,t_{n-1}))
\end{align}

The nested integrals can then be computed using a Gauss-Jacobi quadrature.
