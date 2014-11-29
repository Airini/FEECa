{-# LANGUAGE MultiParamTypeClasses #-}
module Spaces where

class Field v where
  add :: v -> v -> v
  addId :: v
  addInv :: v -> v
  
  mul :: v -> v -> v
  mulId :: v
  mulInv :: v -> v

class Field f => VectorSpace v f where
  --type Vector = v
  vspaceDim :: v -> Int
  addV  :: v -> v -> v
  sclV  :: f -> v -> v
 
class (VectorSpace v f) => Algebra v f where
  addA :: v -> v -> v
  (/\) :: v -> v -> v
  sclA :: f -> v -> v

class (VectorSpace v f) => Function h v f where
  deriv :: v -> h -> h
  eval  :: v -> h -> f

