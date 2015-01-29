{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module DiffForms where

import Forms
import Spaces
import Polynomials
import Utility (pairM)
import Control.Applicative


-- Maybe should wrap polynomials: finite degree => Field too + defined dimensionality
instance (Field f) => VectorSpace (Polynomial f) where
  type Fieldf (Polynomial f) = f
  vspaceDim = indets
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
-- sclP x (Polynomial ms) = Polynomial $ map (\(c,is) -> (mul x c, is)) ms

-- | Multiply two polynomials
mulP :: Field a => Polynomial a -> Polynomial a -> Polynomial a
mulP (Polynomial ms) (Polynomial ns) = undefined

-- | Constant == 0 polynomial in n indeterminates
--zerP :: Field a => Int -> Polynomial a
--zerP n = Polynomial [(addId, replicate n 0)]
-- or: much better (needn't have the indeterminate number) + same as the other abstraction
zerP :: Polynomial a
zerP = Polynomial []

instance (Field f) => Field (Polynomial f) where
  add     = addP
  addId   = zerP
  addInv  = undefined -- deg0P ... :S dims
  mul     = undefined -- mulP
  mulId   = undefined -- deg0P ... :S dims
  mulInv  = undefined
  fromInt = undefined -- deg0P ... :S dims

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
  type Fieldf (PolyN f) = f
  vspaceDim _ = undefined
  addV   = add
  sclV a = fmap (mul a)
  --sclV a ZerP       = ZerP
  --sclV a (CttP b)   = CttP (mul a b)
  --sclV a p@(Poln _) = fmap (mul a) p

instance (Field f) => Field (PolyN f) where
  -- add ZerP p = p
  -- add p ZerP = p -- (Poln x p) (Poln y q) | x == y = Poln n $ addP p q
  add (Poln p) (Poln q)     = Poln $ add p q
  add (CttP a) (CttP b)     = CttP (add a b)
  add (CttP a) p@(Poln p')  = add (Poln $ deg0P (indets p') a) p -- Poln $ addP (deg0P (indets p) a) p
  add p@(Poln _) c@(CttP _) = add c p

  addId  = pure addId
  addInv = fmap mulInv -- (Poln p) = Poln $ sclP (mul addId (addInv addId)) p -- lift...

  -- recode: fmap
  -- mul ZerP _ = ZerP
  -- mul _ ZerP = ZerP
  mul (Poln p) (Poln q)     = Poln $ mulP p q
  mul (CttP a) p            = fmap (mul a) p --sclV (mul a) --(CttP b) = CttP (mul a b)
  -- mul p (CttP a) = fmap (mul a) p
  mul p@(Poln _) c@(CttP _) = mul c p
  
  mulId     = pure mulId
  mulInv    = undefined
  fromInt x = CttP (fromInt x)

-- does it even make sense? cannot see immediate use
instance Applicative PolyN where
  pure = CttP
  (<*>) = undefined


-- | Differential forms
--data DiffForm f where
--  DForm :: Field f => Form (PolyN f) -> DiffForm f
type DiffForm f = Form (PolyN f)
-- Immediately "inherits" all class instantiations for 'Form' when an
-- appropriate 'f' is used for Polynomial coefficients

--instance (Field f) => VectorSpace (DiffForm f) where
--  type Fieldf (DiffForm f) = PolyN f
--  addV      = addV -- (addV :: Form (PolyN f) -> Form (PolyN f) -> Form (PolyN f))
--  vspaceDim = undefined
--  sclV      = undefined

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

--evalPn _ (CttP x) = x
evalPn v (CttP c) = eval v $ deg0P (vspaceDim v) c -- (length $ toList v) c
evalPn v (Poln p) = eval v p

--derivPn :: (Rn v, f ~ (Fieldf v), Function (Polynomial f) v f) => v -> PolyN f -> PolyN f
derivPn v (CttP c) = Poln $ deriv v (deg0P (length $ toList v) c)
derivPn v (Poln p) = Poln $ deriv v p

--eg1 = eval [-0.1,10,0] expression

instance (Function (Polynomial f) v) => Function (PolyN f) v where
  type Values (PolyN f) v = Values (Polynomial f) v
  deriv v (CttP c) = Poln $ deriv v (deg0P (vspaceDim v) c)
  deriv v (Poln p) = Poln $ deriv v p
  eval v (CttP c) = eval v $ deg0P (vspaceDim v) c -- (length $ toList v) c
  eval v (Poln p) = eval v p


{-
instance (Rn v, a ~ (Fieldf v), Floating a, Eq a) => Function (PolyN a) v a where
  deriv = derivPn
  eval  = evalPn
-}


