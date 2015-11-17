At the heart of all finite element techniques is finite dimensional functions space, that makes the finding of solutions for differential equations tractable.
%
The spaces used in FEEC are the polynomial spaces of given degree $r$ or less over a simplex $T = [\vec{v}_0,\ldots,\vec{v}_k]$.
%

A general polynomial $f(\vec{x})$ over $\R{n}$ can be written as a linear combination of monomials of the form

\begin{align}\label{eq:mon}
  f(\vec{x}) = \sum_i c_{\vec{\alpha}} \vec{x}^{\vec{\alpha}}
\end{align}

Here, $\vec{\alpha} = (\alpha_0,\ldots,\alpha_{n-1}) \in \mathrm{N}_0^{n}$ is a multi-index and the power of $\vec{x} = (x_0,\ldots,x_{n-1})$ with respect to a multi-index is defined as

\begin{align}
  \vec{x}^{\vec{\alpha}} = \prod_{i=0}^{n-1}x_i^{\alpha_i}
\end{align}

The degree $||\vec{\alpha}||$ of a multi-index is just the sum of its components.

In FEEC, two representations of polynomials in $\R{n}$ are used. The first one, as given by \eqref{eq:mon}, will be referred to as the (standard) monomial basis.
%
The second one uses the barycentric coordinates over a given simplex as basis.
%
Polynomials of this type are referred to as Bernstein polynomials.
%
\subsection{General Polynomials}

FEEC provides the |Polynomial| data type, that implements polynomials using the
standard monomial basis but also provides functionality for the representation
of general polynomials over an arbitrary monomial basis.

\begin{code}
data Polynomial a =
    Polynomial {  degree  :: Int,
                  terms   :: [Term a] }
    deriving (Eq, Show)
\end{code}

The |Polynomial| type takes as type parameter the data type representing the Ring the polynomials are to be defined over.
%
Polynomials are represented as a list of terms, where a term is either a constant scalar or a scalar and multi-index representing a scaled monomial.
%
By separating functions that make assumption about the specific form of the underlying monomials from others that do not, the |Polynomial| type can easily be used as a building block over different bases.

The |Polynomial| type also provides a concrete implementation of polynomials using the standard monomial basis.
%
As such, it is an instance of the |Function| type class and thus supports evaluation and derivation of polynomials.
%
The algebraic structure of the polynomials is represented by the |Ring| and |VectorSpace| type classes.


\subsection{Bernstein Polynomials}

Define the Bernstein monomial of degree $r$

\begin{align}
  B^{\vec{\alpha}}_r &= \frac{n!}{\vec{\alpha}!} \prod_{i=0}^k \lambda_i^{\alpha_i}(\vec{x})
\end{align}
where the factorial of a multi-index is defined of the product of the factorial of its components and $||\vec{\alpha}|| = r$.
%
A Bernstein polynomial is then a linear combination of Bernstein monomials.
%
Since a Bernstein polynomial is defined over barycentric coordinates, it can only be defined with respect to a given Simplex.
%
In FEEC, Bernstein polynomial are represented by the |BernsteinPolynomial| type.
%
The |BernsteinPolynomial| type is parametrized by two types, that specify the scalar type and the vector type that is used for the representation of the simplex.

\begin{code}
data BernsteinPolynomial v r =
        Bernstein (Simplex v) (Polynomial r)
     |  Constant r
    deriving (Eq, Show)
\end{code}

A common Bernstein polynomial is represented by the simplex it is defined over and a general polynomial.
%
In addition to this, the |Constant| type constructor is used to represent constant Bernstein polynomials that can be defined without reference to a specific simplex.
%
The arithmetic operations over Bernstein polynomials are the same as for the polynomials defined over the canonical basis.
%
\subsubsection{Extension of Bernstein Polynomials}

Since Bernstein polynomials can also be defined over subsimplices of a given simplex, an important operation is the extension of the Bernstein polynomials defined on the subsimplex to the supersimplex.
%
The extension of a Bernstein monomial from a face of a simplex given by the increasing map $\sigma$ to the simplex is defined by
\begin{align}
  B_r^{\vec{\alpha}} = \prod_{i = 0}^k \lambda_{\sigma(i)}^{\alpha_i}
\end{align}

The generalization to Bernstein polynomials is then straight-forward.
