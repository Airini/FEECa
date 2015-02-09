{-# LANGUAGE FlexibleContexts #-}
module Mask where

import Spaces
import Polynomials
import Forms
import DiffForms
import PolyN
import Control.Applicative
import Vector
import Point


type Monop t = t -> t
type Binop t = t -> t -> t

(.*) :: VectorSpace v => Fieldf v -> v -> v
(.*) = sclV

(.+.) :: VectorSpace v => Binop v
(.+.) = addV

(.+) :: Field f => Binop f
(.+) = add

º :: Field f => f
º = addId

(.-) :: Field f => Monop f
(.-) = addInv

(·) :: Field f => Binop f
(·) = mul

ı :: Field f => f
ı = mulId

(¬) :: Field f => Monop f
(¬) = mulInv

(÷) :: Field f => Binop f 
a ÷ b = a · mulInv b

(†) :: Algebra a => Binop a
(†) = addA

(∂) :: Function h v => v -> h -> h
(∂) = deriv

dx :: Field f => Int -> Form f
dx = oneForm

(#) :: Form Double -> [Vector] -> Double
(#) = refine dxV

--d' :: (Function h v, Algebra (Form h)) => (Int -> v) ->  Monop (Form h)
--d' = df'

coordinate :: Field a => Int -> Int -> [a]
coordinate i n = replicate (i-1) addId ++ mulId: replicate (n-i) addId

-- For now: dimensioned passed in
d :: Int -> Monop (Form (PolyN Double))
d n = df' (vector . flip coordinate n) --(vector $ coordinate 0 2)

(§) :: Form (PolyN Double) -> Point -> Form Double
(§) = evalDF


