{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Spaces where

class Field v where
  add     :: v -> v -> v
  addId   :: v
  addInv  :: v -> v
  
  mul     :: v -> v -> v
  mulId   :: v
  mulInv  :: v -> v

  -- associative, distributive, ...

  -- Compared to Num: add = (+), mul = (*), addId = 0, mulId = 1, addInv = negate, but no mulInv
  -- Fractional: mulInv = recip
  -- Missing: fromInteger, or rather fromRational

class VectorSpace v where
  type Fieldf v :: *      -- Coefficient field
  vspaceDim :: v -> Int   -- Morally an associated constant, could be modelled with a type :: Nat
  addV  :: v -> v -> v
  sclV  :: Fieldf v -> v -> v

instance Field Double where
  add    = (+)
  addId  = 0
  addInv = (0-)
  
  mul    = (*)
  mulId  = 1
  mulInv = (1/)

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


