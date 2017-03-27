{-# LANGUAGE FlexibleContexts #-}

module FEECa.DifferentialForm (

  -- * Differential form type
    DifferentialForm

  -- * Mathematical differential form operations
  , df, eval

  -- * Projection operations: into scalara and polynomial values respectively
  , dxV

  ) where

-- import Control.Applicative (pure)
import FEECa.Internal.Form
import FEECa.Internal.Spaces
import FEECa.Internal.Vector


-- | Differential forms
type DifferentialForm f = Form f
  -- DForm :: (Function h v) => Form h -> DifferentialForm h
-- Immediately "inherits" all class instantiations for 'Form' when an
-- appropriate 'f' is used for Polynomial coefficients (?)

--data DifferentialForm f where
--  DForm :: Ring f => Form (PolyN f) -> DifferentialForm f


-- Few examples to test how to write
{-f :: Ring f => DifferentialForm f
f = oneForm 1 4

g :: Ring f => DifferentialForm f
g = sclV (add mulId mulId) (oneForm 2 3)

h :: Ring f => DifferentialForm f
h = sclV (constant addId) (oneForm 3 3)

t :: DifferentialForm Double
t = sclV (add (constant 8.9) (linearPolynomial [0,2,3])) (oneForm 1 5)

b :: Vector Double
b = vector [1,2,0]
y :: Vector Double
y = vector [3,-2.3,1]
-}

dxV :: Field f => Int -> Vector f -> f
dxV i x = toList x !! (i-1)

{-expression = refine dxVP (t /\ g) [b, y]

eg1 = evaluate (vector [-0.1,10,0]) expression
-- -479.74
-}

-- basisIx must be in agreement with the proj paramenter used in evaluation!
--      OR: add zero-th vector to 'VectorSpace' class?
-- Remark: reduced generality for our R^n types

-- Generalised to any appropriate form (polynomial differential forms being but
-- a case)
df :: (Function h v, Algebra (Form h)) => (Int -> v) -> Form h -> Form h
df basisIx form =
    foldr (addA . (\i -> fmap (derive (basisIx i)) (oneForm i n /\ form)))
          (zeroForm (1 + arity form) n)
          [1..n]
  where n = dimVec form

-- b1 i = replicate (i-1) addId ++ mulId:replicate (3-i) addId

eval :: (Function h (Vector f)) => DifferentialForm h -> Vector f -> Form f
eval u = ($u) . fmap . evaluate
