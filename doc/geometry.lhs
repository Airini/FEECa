%------------------------------------------------------------------------------%
As described above, finite element exterior calculus is based on the
 discretization of arbitrary domains in Euclidean space.
%
Let $\Omega \in \R{n}$ be such a domain.
%
The discretization of $\Omega$ is achieved by tesselation using primitive
geometrical shapes, the so-called finite elements.
%
The two types of finite elements used in finite element exterior calculus
are simplices and cubes, of which up to now only simplices are supported in FEECa.
%
The representation of these geometrical structures is realized in FEECa using the
|EuclideanSpace| type-class as well as the |Vector| and |Simplex| data types.

\subsection{Vectors}
\label{subsec:vectors}

%
Vectors in $\mathrm{R}^n$ are represented by the multi-parameter typeclass |EuclideanSpace v r|, where |v| represents the vector type and |r| the scalar type.
%
The superclass |VectorSpace| represents the arithmetic structure of a general vector space over a ring of scalars.
%
The |EuclideanSpace| class is basically an extension of the |VectorSpace| class that adds the inner product in $\mathrm{R}^n$ and the representation as finite lists of vector components to the functionality provided by the |VectorSpace| class.
%**TODO: Why is there an asymmetry between VectorSpace which uses an associated type and EuclideanSpace which uses two type parameters. They are tied together using Scalar v ~ r anyway.
\begin{code}
class (Eq v, Dimensioned v, VectorSpace v,
       Eq r, Field r, Scalar v ~ r)
       => EuclideanSpace v r where
    dot        :: v   -> v -> r
    fromList   :: [r] -> v
    toList     :: v   -> [r]
\end{code}
%
A concrete vector type is implemented by the |Vector a| data-type, which represents a vector using a list |[a]| of type |a|.
%
The parameter type |a| represents the scalar type used for the vector and thus defines
the underlying arithmetic.
%
This enables us to choose between exact and standard floating point arithmetic depending on the application.
%**TODO: I would use newtype here - but perhaps you don't want to spare the readers from too many syntactic concepts?
\begin{code}
data Vector a = Vector { components :: [a] }
               deriving (Show)
\end{code}

\subsection{Simplices}
%
A $k$-simplex $\smp{T} = [\vec{v_0},\ldots,\vec{v_k}]$ in $n$-dimensional Euclidean space $\R{n}$ is the convex hull of $k+1$ vertices $\vec{v_0},\ldots,\vec{v_k}$ such that the spanning vectors $\vec{v_1}-\vec{v_0} ,\ldots,\vec{v_k}-\vec{v_0}$ are linearly independent.
%
A subsimplex, or face $f$, of dimension $k'$ is a simplex consisting of the convex hull of a subset of $k' + 1$ vertices of its supersimplex.
%
An important concept in finite element exterior calculus is the representation of faces of simplices using an increasing map $\sigma: \{0,\ldots,k'\} \mapsto \{0,\ldots,k\}$.
%
In the code, such a map is represented as an increasing list of type |[Int]|.
%
A simplex is represented by a list of vertices and such an increasing map.
%
Keeping track of $\sigma$ for subsimplices is important to later extend polynomials from the face of a simplex to the full simplex.

\begin{code}
data Simplex a =  Simplex {  sigma :: [Int],
                             vertices :: [a] }
                deriving (Eq, Show)
\end{code}

\subsubsection{Barycentric Coordinates}

The barycentric coordinates $\lambda_0,\ldots,\lambda_k$ defined over a simplex $\smp{T} = [\vec{v}_1,\ldots,\vec{v}_k]$ describe a point $\vec{x}$ on a simplex as a convex combination of the $k+1$ vertices:

\begin{align}
  \vec{x} = \lambda_0 \vec{v_0} + \ldots + \lambda_k \vec{v_k}
\end{align}

Viewed as a function of $\vec{x}$, the barycentric polynomials form a basis for the space of affine functions on the simplex.
%
The barycentric coordinates obviously satisfy
\begin{align}
 \lambda_i(\vec{v}_j) = \delta_{ij}
\end{align}
for $i=0,\ldots,k; j = 0,\ldots,k$, $\vec{v}_j$ the vertices of the simplex and $\delta_{ij}$ the Kronecker delta.
%
 This property results in a linear system of equations for the coefficients of the affine functions which can be solved to obtain the representation of the barycentric coordinates as an affine function in the components of $\vec{x}$.
%
\subsubsection{Integrals over Simplices}

An important operation in finite element exterior calculus is the computation of integrals over a finite element.
%
To compute the integral of an arbitrary function over a given simplex $T$, we use the technique described in \cite{Ainsworth}.
%
By a suitable coordinate transform, the integral of a function $f(\vec{x})$ over the simplex $T$ can be written as
%**TODO: I don't see the pattern hidden in the \ldots

\begin{align}
  V(\smp{T}) \int_0^1 dt_1(1-t_1)^{n-1}\ldots\int_0^1 dt_nf(\vec{x}(t_0,\ldots,t_{n-1}))
\end{align}

The nested integrals can then be computed using a Gauss-Jacobi quadrature.
