{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module FEEC.Internal.Spaces where

class Ring v where
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


instance Ring v => Num v where
  (+) = add
  (*) = mul
  negate = addInv
  fromInteger = fromInt
  abs = undefined -- XXX: :S TODO: add Ord v to constraint? In any case, for polynomials...
  signum = undefined -- XXX: :S
  -- Compared to Num: add = (+), mul = (*), addId = 0, mulId = 1, addInv = negate

instance Ring Double where
  add = (+)
  addId = 0
  addInv = (0-)
  mul = (*)
  mulId = 1
  fromInt = fromIntegral
  pow = (^^)

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

-- Maybe not necessary to have
class (VectorSpace v, Ring (Values h v)) => Function h v where -- h ~= v -> Values h v
  type Values h v :: *
  type GeomUnit h v :: *  -- not the best option: will lead to different class instantiations, per geometrical object => not possible here"
  -- Suggestion : new class? ADT to represent them?

  derive    :: v -> h -> h
  integrate :: GeomUnit h v -> h -> Values h v

  evaluate  :: v -> h -> Values h v

  ($$)  :: h -> v -> Values h v
  ($$) = flip evaluate

class Dimensioned t where
  dim :: t -> Int

