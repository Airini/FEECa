{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Spaces where

class Field v where
  add     :: v -> v -> v
  addId   :: v
  addInv  :: v -> v

  mul     :: v -> v -> v
  mulId   :: v
  mulInv  :: v -> v

instance Floating a => Field a where
    add = (+)
    addId = 0
    addInv = (0-)
    mul = (*)
    mulId = 1
    mulInv = (1/)
  -- associative, distributive, ...

  -- Compared to Num: add = (+), mul = (*), addId = 0, mulId = 1, addInv = negate, but no mulInv
  -- Fractional: mulInv = recip
  -- Missing: fromInteger, or rather fromRational

class Eq v => VectorSpace v where
  type Fieldf v :: *      -- Coefficient field
  vspaceDim :: v -> Int   -- Morally an associated constant, could be modelled with a type :: Nat
  addV  :: v -> v -> v
  sclV  :: Fieldf v -> v -> v
  powV :: Integral i => v -> [i] -> (Fieldf v)
  toList :: v -> [Fieldf v]

instance (Floating a, Eq [a]) => VectorSpace [a] where
   type Fieldf [a] = a
   vspaceDim v = length v

   addV [] [] = []
   addV (v:vs) (w:ws) = (add v w) : (addV vs ws)
   addV _ _ = error "addV: Lists do not have equal length"

   sclV _ [] = []
   sclV a (v:vs) = (mul a v) : (sclV a vs)

   powV [] [] = 1
   powV (v:vs) (i:is) = v**(fromIntegral i) * (powV vs is)
   powV _ _ = error "powV: Lists do not have equal length"

   toList v = v

class (VectorSpace v) => Algebra v where -- "union type" of vectorspaces of different dimension
  addA :: v -> v -> v
  (/\) :: v -> v -> v
  sclA :: Fieldf v -> v -> v

  addA = addV
  sclA = sclV

-- Maybe not necessary to have
class (VectorSpace v, Field f) => Function h v f where -- h ~= v -> f
  deriv :: v -> h -> h
  -- integration too
  eval  :: v -> h -> f

  ($$)  :: h -> (v -> f)
  ($$) = flip eval


