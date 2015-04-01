{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module PolyN (
  PolyN(Poln)
  , zerP, deg1, poly, constant
  , indets) where

import Polynomials hiding (constant)
import MultiIndex
import Spaces

import Control.Applicative

import Utility (pairM)


-- Maybe should wrap polynomials: finite degree => Field too + defined dimensionality

-- * Underlying polynomials extras. TODO: migrate

-- | Gives the number of indeterminate variables of a polynomial
indets :: Polynomial f -> Int
indets (Polynomial _ ((_,is):_)) = dim is
-- zero if none??
indets _                       = error "indets: No monomials defining the number of indeterminates"

-- | Scaling of a polynomial
sclP :: Field a => a -> Polynomial a -> Polynomial a
sclP x = fmap (mul x)

-- | Polynomial multiplication
mulP :: Field a => Polynomial a -> Polynomial a -> Polynomial a
mulP (Polynomial n ms) (Polynomial _ ns) = Polynomial n [termMul x y | x <- ms, y <- ns]
  where termMul (a,as) (b,bs) = (mul a b, addMI as bs)


-- * Polynomials for use in forms

-- | Our working polynomial type: defined over some fixed number of indeterminates
--   (even though not explicitly stated) and ready for space instantiation
--   NB: a 'Field' instance can now be made since we can have the operation units
--       (constants) independently of the number of indeterminates)
data PolyN f where
    Poln :: {- Dim -> -} Polynomial f -> PolyN f
    CttP :: f -> PolyN f

instance (Show (Polynomial f), Show f) => Show (PolyN f) where
  show (Poln p) = show p
  show (CttP a) = show a


-- For explicit polynomials
poly :: [(f,MultiIndex)] -> PolyN f
poly = Poln . (Polynomial undefined)

constant :: f -> PolyN f
constant = pure

-- | Constant == 0 polynomial in n indeterminates
zerP :: PolyN a
zerP = Poln $ Polynomial undefined []

deg1 :: Field f => [f] -> PolyN f
deg1 = Poln . deg1P


-- * Implementation related instances

-- | 'PolyN' type as a functor
instance Functor PolyN where
  fmap f (Poln p) = Poln (fmap f p)
  fmap f (CttP a) = CttP $ f a
  -- fmap f ZerP     = ZerP

-- does it even make sense? cannot see immediate use
instance Applicative PolyN where
  pure = CttP
  (<*>) (CttP f)  = fmap f
  (<*>) (Poln fs) = undefined -- Poln . fmap (concatMap (pairM ($) addMI) fs) -- map each function + add its indices?


ttest u v w = pure (.) <*> u <*> v <*> w

-- * Math related instances

-- | 'PolyN' as a vector space
instance (Field f) => VectorSpace (PolyN f) where
  type Fieldf (PolyN f) = Fieldf (Polynomial f)  -- OR: f
  vspaceDim _ = undefined
  addV   = add
  sclV a = fmap (mul a)

-- | 'PolyN' as a field
instance (Field f) => Field (PolyN f) where
  -- add ZerP p = p
  -- add p ZerP = p -- (Poln x p) (Poln y q) | x == y = Poln n $ addP p q
  add (Poln p) (Poln q)     = Poln $ addV p q  -- addP
  add (CttP a) (CttP b)     = pure (add a b)
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
  fromInt x = pure (fromInt x)

-- | 'PolyN' as a function type with directional derivative and point-evaluation
instance (Function (Polynomial f) v) => Function (PolyN f) v where
  type Values (PolyN f) v = Values (Polynomial f) v

  -- Since we specify case by case, maybe it's better to avoid translation of CttP step?
  deriv v (CttP c) = Poln $ deriv v (deg0P (vspaceDim v) c)
  deriv v (Poln p) = Poln $ deriv v p
--deriv _ (CttP )  = CttP addId
  eval v (CttP c)  = eval v $ deg0P (vspaceDim v) c -- (length $ toList v) c
  eval v (Poln p)  = eval v p
--eval _ (CttP x)  = x

-- NB: the fewer times we need to rely on v, the vector space type we act on,
--   the better (eg: vspaceDim...)

