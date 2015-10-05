%format cong = "\cong "

%------------------------------------------------------------------------------%
With the purpose of setting a standard collection of abstract constructs and
structures to shape the package, a series of backbone classes are defined in the
internal module \module{Spaces}.
%

\subsection{Structured sets of elements at the base}

The objects employed in finite element exterior calculus, such as alternating
and differential forms, are objects which are generally given over some field
$K$.
%
However, it is possible to be more general and give the same constructions
over a (commutative) ring since the implementation of the algebraic operators
we will need do not require the notion of division.

The operations forming the |Ring| class definition are those expected from a
ring: addition |add| and multiplication |mul|.
%
A canonical representation of the identities to both operations, |addId| =
$0$ and |mulId| = $1$ respectively, as well as an additive inverse operation
|addInv| complete the definition of a |Ring|.

> class Eq v => Ring v where
>   add     :: v -> v -> v
>   addId   :: v
>   addInv  :: v -> v
>  
>   mul     :: v -> v -> v
>   mulId   :: v

\paragraph{}

This permits generalising the represented concepts to other structures such as
Kähler differentials [cite].
%
It also opens the possibility to experiment with different precision floating
point number representations or even support with some small additional work
symbolic representations which can later be compiled down to more efficient code
in another paradigm.

\paragraph{}

Nonetheless, a completion from the |Ring| class to a class for fields |Field| is
reflected via another class which, remaining conceptually correct in the
mathematical sense, require the type to be a |Ring| type already.

> class Ring f => Field f where
>   mulInv     :: f -> f


\subsection{Algebraic structures}

Building on the |Ring| class, the |VectorSpace| class is defined.
%
Per se, it in reality formally corresponds to the class of modules over rings,
since the coefficients scaling its vector elements are in fact a |Ring| type.
%
A dependency between the vector space type (that is, the type of its vectors)
and the coefficients is created via a type association |Scalar|.
%
This entails a functional dependency by which the coefficient type is determined
uniquely by the vector type in any one instance of the package.
%
A generalisation from here might be possible, although maybe not necessary for
practical use of the package.

> class (Ring (Scalar v)) => VectorSpace v where
>   type Scalar v :: *
>   addV  :: v -> v -> v
>   sclV  :: Scalar v -> v -> v

The typical vector addition and scaling operations must be provided by the
instance of a type as a |VectorSpace|.

Other than the typical \R{n} space, many of the central structures provided by
the package to implement finite element exterior caculus give rise to a
|VectorSpace| type, such as forms themselves (in \refSec{sec:forms}) and the
different polynomial implementations (in \refSec{sec:polyns}).
%
|VectorSpace| is hence a backbone class to much of the implemented functionality
and allows for it to be expressed in a general way, leading to flexibility in
specific instantiation of the deployed types.

\paragraph{}

To provide a generic interface for exterior algebra operations, whether for
alternating or differential forms (and these defined over some arbitrary
function representation - different possible kinds of polynomials in the
package), an additional class |Algebra| is defined.
%
To keep consistency, it requires for its type to be of |VectorSpace| class
already, via which it provides a built-in implementation of addition and scaling
of its elements (that which it inherits from the vector space characterisation).

> class VectorSpace v => Algebra v where
>   addA :: v -> v -> v
>   (/\) :: v -> v -> v
>   sclA :: Scalar v -> v -> v
>   addA = addV
>   sclA = sclV

Of course, instances cannot be arbitrary: not all |VectorSpace| types will
necessarily have an associated product, in our case the exterior product.
%
All instances of operations over these algebraic structures should respect the
laws that rule over them, a property that cannot be demonstrated in a foolproof
way but at least can be reliably checked with automated random testing.

Concretely, the algebras we will represent (those of alternating and
differential forms) are anti-commutative graded algebras \[cite A\].
%
In our frame, these algebras are composed by the direct sum of vector spaces of
different dimensions (the spaces of forms of varying arities, see
\refSec{sec:forms} for more).

\paragraph{}

Due to the difficulty in working with promoted types in practical applications
(particularly when support for random generation of test cases is sought, as is
our case, for implementation verification), an aspect of these spaces was not
implemented in the most secure way: dimensionality.

Frequently, a general datatype may serve as representation to several algebraic
structures sharing the central feature in their construction that gives them
such a structure.
%
The datatype may be indexed by kinded naturals according to dimension so as to
guarantee invariants in the implementation.
%
As a work-around, a smaller class |Dimensioned| is defined for elements from
which the dimension of their space may be extracted.
%
This general interface to a notion of dimensionality is necessary to provide
generic operations that rely on a canonical basis of a fixed space.

> class Dimensioned t where
>   dim :: t -> Int


\subsection{Getting closer to the domain of application}

U




