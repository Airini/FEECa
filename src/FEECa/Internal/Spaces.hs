{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}

module FEECa.Internal.Spaces where

import Numeric (fromRat)


-- | Class of types spaces forms and finite element spaces will be based upon.
-- Mathematically these should be fields, but inversion is not required in our
-- context, hence the simplification of properties and operations to that which
-- will be necessary for the operations aimed to be supported: those of a ring
class Eq v => Ring v where  -- XXX: only Eq v for now
  add     :: v -> v -> v
  addId   :: v
  addInv  :: v -> v

  mul     :: v -> v -> v
  mulId   :: v

  fromInt :: Integral a => a -> v  -- XXX: or fromRational?

  pow     :: Integral a => v -> a -> v
  -- no neg security!
  pow _ 0 = mulId
  pow t n = mul t (pow t (n-1))

fromInt' :: Ring a => Int -> a
fromInt' = fromInt

{-
-- Class instantiation for notation ease.
instance {-# OVERLAPPABLE #-} Ring v => Num v where
  (+) = add
  (*) = mul
  negate = addInv
dd  fromInteger = fromInt
  abs = undefined -- XXX: :S TODO: add Ord v to constraint? In any case, for polynomials...
  signum = undefined -- XXX: :S
-}

-- | Derived subtraction operation from 'Ring' class functions.
sub :: Ring r => r -> r -> r
sub a b = add a (addInv b)

  -- associative, distributive, ...
  -- Compared to Num: add = (+), mul = (*), addId = 0, mulId = 1, addInv = negate

-- | Completion of the 'Ring' class to that of a 'Field'
class Ring f => Field f where
    mulInv     :: f -> f
    fromDouble :: Double -> f
    toDouble   :: f -> Double

-- | Derived division operation from 'Field' class functions.
divide :: Field f => f -> f -> f
divide a b = mul a (mulInv b)

-- Example instances for the floating 'Double' and for the 'Rational' types.
instance Ring Double where
  add     = (+)
  addId   = 0
  addInv  = (0-)
  mul     = (*)
  mulId   = 1
  fromInt = fromIntegral
  pow     = (^^)

instance Ring Integer where
  add     = (+)
  addId   = 0
  addInv  = negate
  mul     = (*)
  mulId   = 1
  fromInt = fromIntegral
  pow     = (^)

instance Field Double where
  mulInv     = (1/)
  fromDouble = id
  toDouble   = id

instance VectorSpace Double where
  type Scalar Double = Double
  addV = (+)
  sclV = (*)

instance Ring Rational where
  add     = (+)
  addId   = 0
  addInv  = (0-)
  mul     = (*)
  mulId   = 1
  fromInt = fromIntegral
  pow     = (^)

instance Field Rational where
  mulInv      = (1/)
  fromDouble  = realToFrac
  toDouble    = fromRat

-- | Definition of vector spaces via a class 'VectorSpace' differing from the
-- mathematical definition in the object it is defined over: 'Ring' scalars
-- instead of 'Field' ones; that is, a module over a 'Ring' in reality.
class (Ring (Scalar v)) => VectorSpace v where --(Scalar v)) => VectorSpace v where -- Module over a Ring
  type Scalar v :: *      -- Coefficient ring
  addV  :: v -> v -> v
  sclV  :: Scalar v -> v -> v -- Scalar v -> v -> v

  -- | Derived vector subtraction from 'VectorSpace' class functions.
  subV :: v -> v -> v
  subV v1 v2 = addV v1 (sclV (addInv mulId) v2)

-- | Zero vector
-- XXX: NB: we could add it as a member of the class?? for more "peculiar" types
zeroV :: VectorSpace v => v -> v
zeroV = sclV addId

-- Example instance of lists as a 'VectorSpace' class type.
{- removed for now: shall we only have _data_ types instantiated as VectorSpace?
(cfr: tt) ==> likely to bring this instance back, just testing-}
--- XXX: maybe it's better ot have Module for Ring, VectorSpace for Field
instance (Ring a, Eq [a]) => VectorSpace [a] where
  type Scalar [a] = a

  addV []     []      = []
  addV (v:vs) (w:ws)  = add v w : addV vs ws
  addV _      _       = error "addV: Lists do not have equal length"

  sclV _ []     = []
  sclV a (v:vs) = mul a v : sclV a vs

-- | Definition of the mathematical 'Algebra' taking our defined 'VectorSpace'
-- class hence being defined over 'Ring's.
class (VectorSpace v) => Algebra v where -- "union type" of vectorspaces of different dimension
  addA :: v -> v -> v
  (/\) :: v -> v -> v
  sclA :: {-r -> v -> v -} Scalar v -> v -> v

  addA = addV
  sclA = sclV

-- | Type class for objects with some notion of dimension associated to them.
class Dimensioned t where
  dim :: t -> Int

-- | Completion of 'VectorSpace' to 'EuclideanSpace' via an inner product for
-- types which may be interpreted as lists of values (that is, they have an
-- immeidate definition as coordinates wrt to a basis) and for appropriate inner
-- products giving rise to metrics.
class (Eq v, Dimensioned v, VectorSpace v, Eq (Scalar v), Field (Scalar v))
    => EuclideanSpace v where
  dot      :: v -> v -> Scalar v
  fromList :: [Scalar v] -> v
  toList   :: v -> [Scalar v]
-- XXX: add basis??

-- | Inner product space to define the generalised inner product on differential
-- | forms.
class (Ring v, VectorSpace v) => InnerProductSpace v where
  inner :: v -> v -> Scalar v

-- Maybe not necessary to have; a bit ad-hoc
-- | Class of simple functions over a 'VectorSpace' and which may be
-- differentiated in any arbitrary direction in it (that is, wrt to an arbitrary
-- vector) and evaluated to 'Scalar' values the 'VectorSpace' is defined over
-- TODO: update so that it requires more? ==> Euclidean Space
class VectorSpace v => Function f v where
  derive    :: v -> f -> f
  evaluate  :: v -> f -> Scalar v

  -- | 'Function' application:
  -- > f $$ v ≈ f $ v
  ($$) :: f -> v -> Scalar v
  ($$) = flip evaluate

-- | 'FiniteElement' class defined over a geometrical type with an associated
-- 'Primitive' type over which functions to be approximated can be evaluated to
-- values of the second parameter type of the class. Types of the class shall
-- provide an evaluation, quadrature-based method.
class FiniteElement t r where
  type Primitive t :: *   -- Q t ~= Primitive t -> r
  -- TODO: also, implicit constraint from integrate:
  --    VectorSpace (Primitive t)
  quadrature :: Int -> t -> (Primitive t -> r) -> r

  -- | Generalised integration expressed in terms of a quadrature method (with
  -- given number of nodes) associated with the 'FiniteElement' type pair.
  integrate :: (r ~ Scalar (Primitive t), Function f (Primitive t))
    => Int -> t -> f -> r
  integrate i t f = quadrature i t (f $$)

-- | A convenient synomym for the neutral vector with respect to addition in an
-- 'EuclideanSpace' of some given dimension.
zero :: EuclideanSpace v => Int -> v -- XXX: shall we use Dimensioned here?
zero n = fromList (replicate n addId)

-- | A convenient synomym for unit vectors with respect to an implicit basis
-- (via indexing of the basis element) in an 'EuclideanSpace' of a given dimension.
unitVector :: EuclideanSpace v => Int -> Int -> v
unitVector n i
    | (n  > 0) && (i >= 0) && (i < n) = fromList $ concat l
    | otherwise = error "unitVector: invalid dimensions!"
  where l = [ replicate i addId, [mulId], replicate (n-i-1) addId ]

-- | General translation to 'EuclideanSpace' vectors from a common
-- representation: lists of 'Double'.
fromDouble' :: EuclideanSpace v => [Double] -> v
fromDouble' = fromList . map fromDouble

-- | General translation from 'EuclideanSpace' vectors to a common
-- representation: lists of 'Double'.
toDouble' :: EuclideanSpace v => v -> [Double]
toDouble' = map toDouble . toList
