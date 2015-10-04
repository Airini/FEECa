{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}

module FEEC.Internal.Spaces where
import Numeric( fromRat )

class Eq v => Ring v where  -- XXX: only Eq v for now
  add     :: v -> v -> v
  addId   :: v
  addInv  :: v -> v
 
  mul     :: v -> v -> v
  mulId   :: v
  
  fromInt :: Integral a => a -> v  -- XXX: or fromRational?
  
  pow     :: Integral a => v -> a -> v
  -- no neg security!
  pow t 0 = mulId
  pow t n = mul t (pow t (n-1))


instance {-# OVERLAPPABLE #-} Ring v => Num v where
  (+) = add
  (*) = mul
  negate = addInv
  fromInteger = fromInt
  abs = undefined -- XXX: :S TODO: add Ord v to constraint? In any case, for polynomials...
  signum = undefined -- XXX: :S

sub :: Ring r => r -> r -> r
sub a b = add a (addInv b)

  -- associative, distributive, ...
  -- Compared to Num: add = (+), mul = (*), addId = 0, mulId = 1, addInv = negate

class Ring f => Field f where
    mulInv     :: f -> f
    fromDouble :: Double -> f
    toDouble   :: f -> Double

divide :: Field f => f -> f -> f
divide a b = mul a (mulInv b)

instance Ring Double where
  add = (+)
  addId = 0
  addInv = (0-)
  mul = (*)
  mulId = 1
  fromInt = fromIntegral
  pow = (^^)

instance Field Double where
    mulInv     = (1/)
    fromDouble = id
    toDouble   = id

instance Ring Rational where
  add = (+)
  addId = 0
  addInv = (0-)
  mul = (*)
  mulId = 1
  fromInt = fromIntegral
  pow = (^)

instance Field Rational where
    mulInv = (1/)
    fromDouble = realToFrac
    toDouble = fromRat

class (Ring (Scalar v)) => VectorSpace v where -- Module over a Ring
  type Scalar v :: *      -- Coefficient ring
  addV  :: v -> v -> v
  sclV  :: Scalar v -> v -> v



-- | General subtraction for vectors
subV :: VectorSpace v => v -> v -> v
subV v1 v2 = addV v1 (sclV (addInv mulId) v2)

-- | Zero vector
-- NB: we could add it as a member of the class?? for more "peculiar" types
zeroV :: VectorSpace v => v -> v
zeroV = sclV addId

{- removed for now: shall we only have _data_ types instantiated as VectorSpace?
(cfr: tt) ==> likely to bring this instance back, just testing-}
instance (Ring a, Eq [a]) => VectorSpace [a] where
  type Scalar [a] = a

  addV [] [] = []
  addV (v:vs) (w:ws) = add v w : addV vs ws
  addV _ _ = error "addV: Lists do not have equal length"

  sclV _ [] = []
  sclV a (v:vs) = mul a v : sclV a vs

class (VectorSpace v) => Algebra v where -- "union type" of vectorspaces of different dimension
  addA :: v -> v -> v
  (/\) :: v -> v -> v
  sclA :: Scalar v -> v -> v

  addA = addV
  sclA = sclV

class Dimensioned t where
  dim :: t -> Int

class (Eq v, Dimensioned v, VectorSpace v, Eq r, Field r, Scalar v ~ r)
    => EuclideanSpace v r where
  dot      :: v   -> v -> r
  fromList :: [r] -> v
  toList   :: v   -> [r]

-- Maybe not necessary to have
class (VectorSpace v) => Function f v where
  derive    :: v -> f -> f
  evaluate  :: v -> f -> Scalar v

  ($$)  :: f -> v -> Scalar v
  ($$) = flip evaluate

class FiniteElement t r where
    type Primitive t :: *
    quadrature :: Int -> t -> (Primitive t -> r) -> r

integrate :: (FiniteElement t r, Function f (Primitive t), Scalar (Primitive t) ~ r)
          => Int
          -> t
          -> f
          -> r
integrate i t f = quadrature i t (f $$)

zero :: EuclideanSpace v (Scalar v) => Int -> v
zero n = fromList (replicate n addId)

unitVector :: EuclideanSpace v (Scalar v) => Int -> Int -> v
unitVector n i
    | (n  > 0) && (i >= 0) && (i < n) = fromList $ concat l
    | otherwise = error "unitVector: invalid dimensions!"
  where l = [replicate i addId, [mulId], replicate (n-i-1) addId]


fromDouble' :: EuclideanSpace v (Scalar v) => [Double] -> v
fromDouble' = fromList . map fromDouble

toDouble' :: EuclideanSpace v (Scalar v) => v -> [Double]
toDouble' = map toDouble . toList
