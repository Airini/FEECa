\section{Vectors}

The \code{Vector} module provides a data type and
functions for the handling of vectors in $n$-dimensional Euclidean space
 $\R{n}$.

%------------------------------------------------------------------------------%

\begin{code}

{-# LANGUAGE TypeFamilies #-}

module FEECa.Internal.Vector(

  -- * The Vector Type
    Vector(..), Dimensioned(..), vector

  -- * Manipulating Vectors
  , toList

  -- * Mathematical Functions
  , dot, pow

  ) where


import Prelude        hiding  ( (<>) )
import FEECa.Utility.Utility  ( sumR )
import FEECa.Internal.Spaces
import FEECa.Utility.Print    as P


\end{code}

%------------------------------------------------------------------------------%

\subsection{The \code{Vector} Type}

 Vectors in $\R{n}$ are represented using the parametrized type \code{Vector a}
 where \code{a} is the number type used to represent the scalars. Internally a
 vector is implemented using a list \code{[a]}. The dimension $n$ is inferred
 from the length of the list. Since vectors from Euclidean spaces of different
 dimension are of the same type, it is up to the user not to mix vectors from
 different spaces, which will lead to runtime errors.

%------------------------------------------------------------------------------%

\begin{code}

-- | Vectors in n-dimensional euclidean space.
data Vector a = Vector { components :: [a] } deriving Show

\end{code}

%------------------------------------------------------------------------------%

 To use the \code{Vector a} type in the \code{FEECa} framework, we have to make
 it an instance of the \code{VectorSpace} and the \code{EuclideanSpace} classes.
 We instantiate two types \code{Vector Rational} and \code{Vector Double} in
 order to have one type with exact arithmetic using Haskells \code{Rational}
 type and one using standard floating point arithmetic.

%------------------------------------------------------------------------------%

\begin{code}

-- | Use numerical equality for floating point arithmetic.
-- instance Eq (Vector Double) where
--    v1 == v2 = and (zipWith U.eqNum (components v1) (components v2))

-- | Use exact equality for exact arithmetic.
--instance Eq (Vector Rational) where
--    v1 == v2 = and (zipWith (==) (components v1) (components v2))

instance Functor Vector where
  fmap f (Vector v) = Vector (fmap f v)

instance Eq a => Eq (Vector a) where
  v1 == v2 = and (zipWith (==) (components v1) (components v2))

  -- TODO: Note that |zipWith| stops when the shorter vector ends, so
  --   for all v. vector []  ==  v
  -- Thus an invariant for this definition to work is that vectors
  -- compared for equality always have the same dimenstion.
  -- TODO: #2 : why is the instance not derived?

-- | R^n as a module.
instance Ring a => Module (Vector a) where
  type Scalar (Vector a) = a
  addV (Vector l1) (Vector l2) = Vector $ zipWith add l1 l2
  sclV c (Vector l) = Vector $ map (mul c) l

  -- Note: Also here, for addV to be correct, it is important that
  -- vectors have the same length (dimension).

instance Field a => VectorSpace (Vector a)

-- | R^n as a Euclidean space.
instance (Field a, Eq a) => EuclideanSpace (Vector a) where
  dot      = dotVector
  toList   = components
  fromList = vector
  proj     = projVector

projVector :: (Field a) => Vector a -> Vector a -> Vector a
projVector = projDefault

dotVector :: Ring r => Vector r -> Vector r -> r
dotVector (Vector l1) (Vector l2) = sumR (zipWith mul l1 l2)

-- | The dimension of vectors.
instance Dimensioned (Vector a) where
  dim (Vector l) = length l

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Constructors}

 The \code{Vector a} data type provides the \code{vector} function as a smart
 constructor for vectors. Even though up to now the constructor does nothing
 but calling the type constructor, the \code{vector} functions is provided to
 hide the internal implementation to other modules.

%------------------------------------------------------------------------------%

\begin{code}

-- | Create vector from list of components.
vector :: [a] -> Vector a
vector = Vector

\end{code}

%------------------------------------------------------------------------------%

\subsubsection{Printing Vectors}

 Vectors can be displayed on the command line in two ways. As an instance of
 \code{Show}, calling \code{show} on a vector will display its type structure.
 Using \code{pPrint} to render the vector, the vector is redered as a column
 vector using unicode characters.

%------------------------------------------------------------------------------%

\begin{code}

-- | Pretty printing for vectors.
instance Field a => Pretty (Vector a) where
  pPrint v = text "Vector in "
              <> rn (dim v)
              <> text ":"
              P.$$ printVector 2 (map toDouble (components v))

\end{code}
