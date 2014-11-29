{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Spaces where

class Field v where
  add :: v -> v -> v
  addId :: v
  addInv :: v -> v
  
  mul :: v -> v -> v
  mulId :: v
  mulInv :: v -> v

class VectorSpace v where
  type Fieldf v :: *
  vspaceDim :: v -> Int
  addV  :: v -> v -> v
  sclV  :: Fieldf v -> v -> v
 
class (VectorSpace v) => Algebra v where
  addA :: v -> v -> v
  (/\) :: v -> v -> v
  sclA :: Fieldf v -> v -> v

class (VectorSpace v) => Function h v where
  deriv :: v -> h -> h
  eval  :: v -> h -> Fieldf v


