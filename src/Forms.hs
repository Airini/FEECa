-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Forms where

import Data.List(intersect)
import Data.Type.Natural
import Spaces
import Discrete


data Vector f = Vex Int [f]

addList :: Field f => Vector f -> Vector f -> Vector f
addList (Vex n xs) (Vex m ys)
  | n /= m = error "addList: vectors must belong to the same space"
  | otherwise = Vex n (zipWith add xs ys)

scaleList :: Field f => f -> Vector f -> Vector f
scaleList a (Vex n xs) = Vex n (map (mul a) xs)

instance Field f => VectorSpace (Vector f) where
  type Fieldf (Vector f) = f
  vspaceDim (Vex n _) = n
  addV = addList
  sclV = scaleList


data Form v f = 
  Fform { arity :: Int, -- vecDim??
          constituents :: [([Int], f)], operator :: [v] -> f }
-- where the f in constituents might very well be changed to (v -> f) so as to
-- englobe differential forms

-- TODO: functor?


dx :: (Field f, VectorSpace v) => Int -> Form v f
dx i | i <= 0    = error "dx: invalid projection of a negative component"
     | otherwise = Fform 1 [([i],mulId)] undefined


-- TODO: missing permutation simplification/identification
(+++) :: (Field f') => Form v f' -> Form v f' -> Form v f'
omega +++ eta
    | arity omega /= arity eta = error "(+++): forms must be of the same dimension"
    | otherwise = Fform (arity eta)
                  (step (constituents omega) (constituents eta))
                  undefined
  where step [] ys = ys
        step xs [] = xs
        step (x:xs) (y:ys)
          | fst x == fst y = (fst x, add (snd x) (snd y)) : step xs ys
          | fst x < fst y  = x : step xs (y:ys)
          | otherwise      = y : step (x:xs) ys

pairM :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairM f h (x,y) = (f x, h y)

(***) :: Field f => f -> Form v f -> Form v f
a *** omega = Fform (arity omega)
                    (map (pairM id (mul a)) (constituents omega))
                    undefined

(//\\) :: Field f => Form v f -> Form v f -> Form v f
omega //\\ eta = Fform (arity omega + arity eta)
                       (concatMap (\d -> map (combine d) (dxs d)) (constituents eta))
                       undefined
  where dxs (ys,_) = filter (null . intersect ys . fst) (constituents omega)
        combine (ys,b) (xs,a)
          | null (intersect xs ys) = (xs++ys, mul a b)
          | otherwise              = ([],addId)


instance (VectorSpace v, Field f) => VectorSpace (Form v f) where
  type Fieldf (Form v f) = f
  vspaceDim _ = undefined
  addV = (+++)
  sclV = (***)

instance (VectorSpace v, Field f) => Algebra (Form v f) where
  addA = addV
  (/\) = (//\\)
  sclA = sclV

-- Our basic projection for 'Vector f'
dxV :: Int -> [Vector f] -> f
dxV i [Vex n x] = x !! i
dxV i _   = error "dxV: incorrect number of arguments; must only be 1"

refine :: (Field f, Num f, VectorSpace v) =>
          (Int -> v -> f)      -- ^ The definition for the projection function
                               --   for the specific vector space
       -> Form v f
       -> Form v f
refine proj (Fform k cs _) = Fform k cs undefined 
  where op = \vs -> foldl (\t -> add t . ($ vs)) addId (map (formify proj) cs)

formify :: (Field f, Num f, VectorSpace v) =>
              (Int -> v -> f) -> ([Int],f) -> [v] -> f
formify proj (i:is, s)
    | null is   = mul s . proj i . head
    | otherwise = \vs ->
        foldl add addId (map (\(w,e) -> mul (mul
                                  ((-1) ^ (sign (w,e)))
                                  ((proj i . head) (choose w vs)))
                                  (formify proj (is,s) (choose e vs)))
                             (permutationPairs (vspaceDim (head vs)) 1 (length is)))
  where choose ns vs = pick (differences ns) vs

zeroForm :: Form v f
zeroForm = Fform 0 [] undefined

contract :: Form v f -> v -> Form v f
contract omega | null (constituents omega) = const zeroForm
               | otherwise                 = undefined

-- We need a basis here
(<>) :: Form v f -> Form v f -> f
omega <> eta = undefined


--- ONLY PLACEHOLDERS!!
data Poly v f = Pp (v -> f)

instance (VectorSpace v, Field f) => Function (Poly v f) v f where
  deriv = undefined
  eval x (Pp g) = g x

instance Field f => VectorSpace (Poly v f) where
  type Fieldf (Poly v f) = f
  vspaceDim = undefined
  addV (Pp g) (Pp h) = Pp $ \vs -> add (g vs) (h vs)
  sclV a (Pp g) = Pp $ \vs -> mul a (g vs)
---



