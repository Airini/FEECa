{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DiffForms (DiffForm, df, evalDF, dxV, dxVP) where

import Forms
import Spaces
import Polynomials
import Utility (pairM)
import Vector
import Point
import Polynomials
import Control.Applicative (pure)


-- | Differential forms
type DiffForm f = Form (Polynomial f)
-- Immediately "inherits" all class instantiations for 'Form' when an
-- appropriate 'f' is used for Polynomial coefficients (?)

--data DiffForm f where
--  DForm :: Field f => Form (PolyN f) -> DiffForm f


-- Few examples to test how to write
f :: Field f => DiffForm f
f = oneForm 1 4

g :: Field f => DiffForm f
g = sclV (add mulId mulId) (oneForm 2 3)

h :: Field f => DiffForm f
h = sclV (Constant addId) (oneForm 3 3)

t :: DiffForm Double
t = sclV (add (constant 8.9) (deg1P [0,2,3])) (oneForm 1 5)

b :: Vector
b = Vector [1,2,0]
y :: Vector
y = Vector [3,-2.3,1]

dxV :: Int -> Vector -> Double
dxV i (Vector x) = x !! (i-1)

dxVP = (fmap . fmap) Constant dxV
expression = refine dxVP (t /\ g) [b, y]

eg1 = eval (Vector [-0.1,10,0]) expression
-- -479.74


-- basisIx must be in agreement with the proj paramenter used in evaluation!
-- TODO: remove ugly v parameter. Ugly possible solution: basisIx 0 returns some "tempalte"
--       arbitrary vector from which dimension can be extracted...
--      OR: add zero-th vector to 'VectorSpace' class?
-- Remark: reduced generality for our R^n types
diff :: (Function (Polynomial Double) v) => (Int -> v) -> v -> DiffForm Double -> DiffForm Double
diff basisIx x form =
    foldr (addA . (\ i -> fmap (deriv (basisIx i)) (oneForm i n /\ form)))
          (zeroForm (1 + arity form) n)
          [1 .. vspaceDim x] -- XXX: shall we use dimVec (n) ?? and so avoid the ugly x
  where n = dimVec form

-- Generalised to any appropriate form (polynomial differential forms being but
-- a case)
df :: (Function h v, Algebra (Form h)) => (Int -> v) -> Form h -> Form h
df basisIx form =
    foldr (addA . (\i -> fmap (deriv (basisIx i)) (oneForm i n /\ form)))
          (zeroForm (1 + arity form) n)
          [1..vspaceDim (basisIx 0)]
  where n = dimVec form

b1 i = replicate (i-1) addId ++ mulId:replicate (3-i) addId

evalDF :: DiffForm Double -> Point -> Form Double
evalDF u = ($u) . fmap . eval . vectify
  where vectify (Point q) = vector q

