{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DiffForms where

import Forms
import Spaces
import Polynomials
import Utility (pairM)
import Control.Applicative


-- Maybe should wrap polynomials: finite degree => Field too + defined dimensionality
instance (Field f) => VectorSpace (Polynomial f) where
  type Fieldf (Polynomial f) = f
  vspaceDim _ = undefined
  addV = addP
  sclV = sclP

instance Functor Polynomial where
  fmap f (Polynomial ms) = Polynomial (map (pairM f id) ms)

-- | Gives the number of indeterminate variables of a polynomial
indets :: Polynomial f -> Int
indets (Polynomial ((_,is):_)) = length is
-- zero if none??
indets _                       = error "indets: No monomials defining the number of indeterminates"

-- | Scaling of a polynomial
sclP :: Field a => a -> Polynomial a -> Polynomial a
sclP x = fmap (mul x)

-- | Multiply two polynomials
mulP :: Field a => Polynomial a -> Polynomial a -> Polynomial a
mulP (Polynomial ms) (Polynomial ns) = undefined

-- | Constant == 0 polynomial in n indeterminates
zerP :: Polynomial a
zerP = Polynomial []


-- | Our working polynomial type: defined over some fixed number of indeterminates
--   (even though not explicitly stated) and ready for space instantiation
data PolyN f where
    Poln :: {- Dim -> -} Polynomial f -> PolyN f
    CttP :: f -> PolyN f
  deriving Show

instance Functor PolyN where
  fmap f (Poln p) = Poln (fmap f p)
  -- fmap f ZerP     = ZerP
  fmap f (CttP a) = CttP $ f a

instance (Field f) => VectorSpace (PolyN f) where
  type Fieldf (PolyN f) = Fieldf (Polynomial f)  -- OR: f
  vspaceDim _ = undefined
  addV   = add
  sclV a = fmap (mul a)

instance (Field f) => Field (PolyN f) where
  -- add ZerP p = p
  -- add p ZerP = p -- (Poln x p) (Poln y q) | x == y = Poln n $ addP p q
  add (Poln p) (Poln q)     = Poln $ addV p q  -- addP
  add (CttP a) (CttP b)     = CttP (add a b)
  add (CttP a) p@(Poln p')  = add (Poln $ deg0P (indets p') a) p -- Poln $ addP (deg0P (indets p) a) p
  add p@(Poln _) c@(CttP _) = add c p

  addId  = pure addId
  addInv = fmap addInv -- (Poln p) = Poln $ sclP (addInv addId) p -- lift...

  mul (Poln p) (Poln q)     = Poln $ mulP p q
  mul (CttP a) p            = fmap (mul a) p
  -- mul p (CttP a) = fmap (mul a) p
  mul p@(Poln _) c@(CttP _) = mul c p
  
  mulId     = pure mulId
  mulInv    = undefined
  fromInt x = CttP (fromInt x)

-- does it even make sense? cannot see immediate use
instance Applicative PolyN where
  pure = CttP
  (<*>) = undefined

instance (Function (Polynomial f) v) => Function (PolyN f) v where
  type Values (PolyN f) v = Values (Polynomial f) v
  
  -- Since we specify case by case, maybe it's better to avoid translation of CttP step?
  deriv v (CttP c) = Poln $ deriv v (deg0P (vspaceDim v) c)
  deriv v (Poln p) = Poln $ deriv v p
--deriv _ (CttP )  = CttP addId
  eval v (CttP c)  = eval v $ deg0P (vspaceDim v) c -- (length $ toList v) c
  eval v (Poln p)  = eval v p
--eval _ (CttP x)  = x

-- | Differential forms
type DiffForm f = Form (PolyN f)
-- Immediately "inherits" all class instantiations for 'Form' when an
-- appropriate 'f' is used for Polynomial coefficients

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


