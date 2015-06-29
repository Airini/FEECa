%------------------------------------------------------------------------------%
%
Finite element exterior calculus is defined on differential forms over a discretized
 domain $\Sigma$ in \R{n}.
%
The discretization of the domain is performed by tesselation into simplices or cubes.
%
Up to now, only simplicial triangulations are supported.
%
The geometrical objects that we are dealing with in \FEEC  are thus points, vectors and
simplices.

\subsection{Points and Vectors}
%
In order to stay close to the mathematical formulation, we distinguish between points and vectors in \R{n}.
%
Points describe fixed positions in \R{n} and are used for example as evaluation points for functions.
%
Vectors describe directions in space and are used for the evaluation of alternating forms.
%
Since these two types of objects are very similar, from an implementation point of view, we define a |Vector| type and use it to define the |Point| type.
%
The |Vector| module provides the |Vector| type, which represents vectors in \R{n} as lists of doubles.
%
Since vectors from Euclidean spaces of different dimension are of the same type, it is up to the user not to mix vectors from different spaces, which will lead to runtime errors.
%
Moreover, the |Vector| module provides functions for the manipulation of vectors, mathematical functions, such as the dot product and exponentiation of vectors, c.f. \S \ref{sec:polyns}.
%


\subsection{Simplices}

