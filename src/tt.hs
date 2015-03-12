{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Prelude as P
import Spaces
import Vector
import Mask

{-
instance (Field f) => P.Num f where
  (+) = add
  (*) = mul
  x - y = add x (addInv y)
  negate = addInv
  abs = undefined     -- ord/eq
  signum = undefined  -- ord/eq
  fromInteger = fromInt
-}

class Op f where
  type Main f :: *
  type Res f :: *
  type Aux f :: *
  (*) :: Aux f -> Main f -> Res f
  (+) :: Main f -> Main f -> Res f

instance VectorSpace v => Op v where
  type Main v = v
  type Res v = v
  type Aux v = Fieldf v
  (*) = sclV
  (+) = addV

-- TODO: what is the conflict?
-- XXX: consier a closed type family for Fieldf ??

instance Field v => Op v where
  type Main v = v
  type Res v = v
  type Aux v = v
  (*) = mul
  (+) = add
{-

instance Field f => VectorSpace f where
  type Fieldf f = f
  vspaceDim _ = 1
  addV = add
  sclV = mul
-}

