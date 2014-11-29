-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Forms where

import Data.List(intersect)
import Spaces

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
  Fform { arity :: Int, constituents :: [([Int], f)], refined :: v -> [v] -> f }
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
  vspaceDim = undefined
  addV = undefined
  sclV = undefined

instance (VectorSpace v, Field f) => Algebra (Form v f) where
  addA = undefined
  (/\) = undefined
  sclA = undefined

