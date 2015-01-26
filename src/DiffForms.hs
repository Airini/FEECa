{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}

module DiffForms where

import Forms
import Spaces
import Polynomials
import Utility (pairM)
import Control.Applicative

type DiffForm f = Form (Polynomial f)

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
indets _                       = error "indets: No monomials defining the number of indeterminates"

-- | Scaling of a polynomial
sclP :: Field a => a -> Polynomial a -> Polynomial a
sclP x = fmap (mul x)
-- sclP x (Polynomial ms) = Polynomial $ map (\(c,is) -> (mul x c, is)) ms

-- | Multiply two polynomials
mulP :: Field a => Polynomial a -> Polynomial a -> Polynomial a
mulP (Polynomial ms) (Polynomial ns) = undefined

-- | Constant == 0 polynomial in n indeterminates
zerP :: Field a => Int -> Polynomial a
zerP n = Polynomial [(addId, replicate n 0)]

data PolyN' f = Pn Int (Polynomial f)

-- :(  Either dependent types (could be hard, just like when we started) or
--     a more flexible implementation for Polynomials:
--       add actual type constructors for deg0P and zerP
instance (Field f) => Field (PolyN' f) where
  add     = undefined -- addP
  addId   = undefined -- zerP :S ==> dependent types
  addInv  = undefined --  :S  ==> rings after all better
  mul     = undefined -- mulP
  mulId   = undefined
  mulInv  = undefined
  fromInt = undefined -- deg0P ...
 
data PolyN f where
  Poln :: {- Dim -> -} Polynomial f -> PolyN f
  ZerP :: PolyN f
  CttP :: f -> PolyN f

instance (Field f) => Field (PolyN f) where
  add ZerP p = p
  add p ZerP = p -- (Poln x p) (Poln y q) | x == y = Poln n $ addP p q
  add (CttP a) (CttP b)     = CttP (add a b)
  add (CttP a) (Poln p)     = Poln $ addP (deg0P (indets p) a) p
  add p@(Poln _) c@(CttP _) = add c p
  add (Poln p) (Poln q)     = Poln $ addP p q

  addId  = ZerP
  addInv = fmap mulInv -- (Poln p) = Poln $ sclP (mul addId (addInv addId)) p -- lift...

  -- recode: fmap
  mul ZerP _ = ZerP
  mul _ ZerP = ZerP
  mul (CttP a) (CttP b) = CttP (mul a b)
  mul (CttP a) (Poln p) = Poln (sclP a p)
  mul p@(Poln _) c@(CttP _) = mul c p
  mul (Poln p) (Poln q)     = Poln $ mulP p q
  
  mulId     = CttP mulId
  mulInv    = undefined
  fromInt x = CttP (fromInt x)

instance (Field f) => VectorSpace (PolyN f) where
  type Fieldf (PolyN f) = f
  vspaceDim _ = undefined
  addV   = add
  sclV a = fmap (mul a)
  --sclV a ZerP       = ZerP
  --sclV a (CttP b)   = CttP (mul a b)
  --sclV a p@(Poln _) = fmap (mul a) p

instance Functor PolyN where
  fmap f (Poln p) = Poln (fmap f p)
  fmap f ZerP     = ZerP
  fmap f (CttP a) = CttP $ f a

-- does it even make sense? cannot see immediate use
instance Applicative PolyN where
  pure = CttP
  (<*>) = undefined


