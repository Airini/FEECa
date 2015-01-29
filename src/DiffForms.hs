{-# LANGUAGE FlexibleContexts #-}

module DiffForms where

import Forms
import Spaces
import Polynomials
import Utility (pairM)

import PolyN

-- | Differential forms
type DiffForm f = Form (PolyN f)
-- Immediately "inherits" all class instantiations for 'Form' when an
-- appropriate 'f' is used for Polynomial coefficients (?)

--data DiffForm f where
--  DForm :: Field f => Form (PolyN f) -> DiffForm f


-- Few examples to test how to write
f :: Field f => DiffForm f
f = dx 1

g :: Field f => DiffForm f
g = sclV (add mulId mulId) (dx 2)

h :: Field f => DiffForm f
h = sclV (CttP addId) (dx 3)

t :: DiffForm Double
t = sclV (add (CttP 8.9) (Poln $ deg1P [0,2,3])) (dx 1)

b :: Vector Double
b = Vex 3 [1,2,0]
y :: Vector Double
y = Vex 3 [3,-2.3,1]

dxVP = (fmap . fmap) CttP dxV
expression = refine dxVP (t /\ g) [b, y]

eg1 = eval [-0.1,10,0] expression
-- -479.74

-- basisIx must be in agreement with the proj paramenter used in evaluation!
-- TODO: remove ugly v parameter. Ugly possible solution: basisIx 0 returns some "tempalte"
--       arbitrary vector from which dimension can be extracted...
--      OR: add zero-th vector to 'VectorSpace' class?
diff :: (Function (PolyN f) v) => (Int -> v) -> v -> DiffForm f -> DiffForm f
diff basisIx x form =
  foldr addA (nullForm (1 + arity form))
             (map (\i -> fmap (deriv (basisIx i)) (dx i /\ form)) [1..vspaceDim x])

-- Generalised to any appropriate form (polynomial differential forms being but
-- a case)
df' :: (Function h v, Algebra (Form h)) => (Int -> v) -> v -> Form h -> Form h
df' basisIx x form =
  foldr addA (nullForm (1 + arity form))
             (map (\i -> fmap (deriv (basisIx i)) (dx i /\ form)) [1..vspaceDim x])


b1 i = replicate (i-1) addId ++ mulId:replicate (3-i) addId

