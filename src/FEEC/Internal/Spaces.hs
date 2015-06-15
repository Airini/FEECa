{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module FEEC.Internal.Spaces where

class Field v where  -- TODO: Actually Ring
  add     :: v -> v -> v
  addId   :: v
  addInv  :: v -> v
  mul     :: v -> v -> v
  mulId   :: v
  mulInv  :: v -> v
  fromInt :: Integer -> v
  pow     :: Integral a => v -> a -> v
  -- no neg security!
  pow t 0 = mulId
  pow t n = mul t (pow t (n-1))

  -- associative, distributive, ...
  -- Compared to Num: add = (+), mul = (*), addId = 0, mulId = 1, addInv = negate, but no mulInv
  -- Fractional: mulInv = recip
  -- Missing: fromInteger, or rather fromRational

instance Field Double where
  add = (+)
  addId = 0
  addInv = (0-)
  mul = (*)
  mulId = 1
  mulInv = recip
  fromInt = fromIntegral
  pow = (^^)

class (Field (Fieldf v)) => VectorSpace v where -- Module over a Ring
  type Fieldf v :: *      -- Coefficient field
-- TODO: perhaps remove in favour of just "dim"
--   vspaceDim :: v -> Int   -- Morally an associated constant, could be modelled with a type :: Nat
  addV  :: v -> v -> v
  sclV  :: Fieldf v -> v -> v

-- | General subtraction for vectors
subV :: VectorSpace v => v -> v -> v
subV v1 v2 = addV v1 (sclV (addInv mulId) v2)

-- | Zero vector
-- NB: we could add it as a member of the class?? for more "peculiar" types
zeroV :: VectorSpace v => v -> v
zeroV = sclV addId


{- removed for now: shall we only have _data_ types instantiated as VectorSpace?
(cfr: tt) ==> likely to bring this instance back, just testing
instance (Field a, Eq [a]) => VectorSpace [a] where
  type Fieldf [a] = a

  addV [] [] = []
  addV (v:vs) (w:ws) = add v w : addV vs ws
  addV _ _ = error "addV: Lists do not have equal length"

  sclV _ [] = []
  sclV a (v:vs) = mul a v : sclV a vs
-}
class (VectorSpace v) => Algebra v where -- "union type" of vectorspaces of different dimension
  addA :: v -> v -> v
  (/\) :: v -> v -> v
  sclA :: Fieldf v -> v -> v

  addA = addV
  sclA = sclV

-- Maybe not necessary to have
class (VectorSpace v, Field (Values h v)) => Function h v where -- h ~= v -> Values h v
  type Values h v :: *
  type GeomUnit h v :: *  -- not the best option: will lead to different class instantiations, per geometrical object => not possible here"
  -- Suggestion : new class? ADT to represent them?

  deriv :: v -> h -> h
  integrate :: GeomUnit h v -> h -> Values h v

  eval  :: v -> h -> Values h v

  ($$)  :: h -> v -> Values h v
  ($$) = flip eval

class Dimensioned t where
  dim :: t -> Int

