\section{Points}

The \module{Point} provides the \code{Point} data type that is used to represent
positions in n-dimensional Euclidean space.

%------------------------------------------------------------------------------%

\ignore{
\begin{code}

module FEEC.Internal.Point(

             -- * The Point Type
             Point, point,

             -- * Convenience functions
             origin, unitPoint,

             -- * Conversion to and from 'Vector' where
             fromPoint, toPoint

                          ) where

import Data.List
import FEEC.Internal.Spaces
import FEEC.Internal.Vector
import FEEC.Utility.Print

\end{code}
}

%------------------------------------------------------------------------------%

\subsection{The Point type}

The \code{Point} data type is build on top of the \code{Vector} data type using
 a \code{Vector} to represent the coordinates of the given point.

%------------------------------------------------------------------------------%

\begin{code}
-- | Points in n-dimensional Euclidean space. A point describes a fixed position
-- | in space and can not be computed with.
newtype Point = Point Vector deriving (Eq, Show)

instance Dimensioned Point where
    dim (Point v) = dim v

instance Pretty Point where
    pPrint (Point v) = text "Point in "
                       <> rn (dim v)
                       <> text ":\n"
                       <> printVector 2 (components v)

-- | Constructor for the Point data type.
point :: [Double] -> Point
point = Point . vector
\end{code}

%------------------------------------------------------------------------------%

\subsection{Convenience Functions}

The \code{Point} data type provides convenience functions for the creation of
 special points in $\R{n}$. The \code{origin} function returns the \code{Point}
 instance representing the origin in $\R{n}$. \code{unitPoint} returns the point
in $\R{n}$ that lies at a distance of 1 from the origin on the given axis.

%------------------------------------------------------------------------------%

\begin{code}

-- | Return the origin in R^n.
origin :: Int -> Point
origin n = Point $ vector (replicate n 0)

-- | Point with the ith component 1.0 and all other components 0.0.
unitPoint :: Int -> Int -> Point
unitPoint n i = Point (unitVector n i)

\end{code}

%------------------------------------------------------------------------------%


\subsection{Conversion to and from Vectors}

The point data type doesn't provide any arithmetic functions. In order to
 perform computations with points it is necessary to convert them to vectors.
 The vector corresponding to a given point is just the vector pointing from the
 origin to the given point.

%------------------------------------------------------------------------------%

\begin{code}

-- | Point corresponding to given position vector.
toPoint :: Vector -> Point
toPoint v = Point v

-- | Position vector of given point.
fromPoint :: Point -> Vector
fromPoint (Point v) = v

\end{code}
