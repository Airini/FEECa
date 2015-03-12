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
coordinate i n = take n $
                  (repeat addId : iterate (addId:) (mulId:repeat addId)) !! i

bssIx n = vector . flip coordinate n

-- or <|> ?
(<>) :: Form Double -> Form Double -> Int -> Double
(<>) omega eta n = inner dxV (bssIx n) omega eta
-- n = max of the dx _i_ ?? enough?

(⌟) :: Form Double -> Vector -> Form Double
(⌟) = contract dxV


𝝹 :: Form (PolyN Double) -> Int -> Form (PolyN Double)
𝝹 form n = contract (const . deg1 . flip coordinate n) form (undefined::Vector)
-- TODO: extract degree from polynomial

-- For now: dimensioned passed in
d :: Int -> Monop (Form (PolyN Double))
d n = df (vector . flip coordinate n) --(vector $ coordinate 0 2)

(§) :: Form (PolyN Double) -> Point -> Form Double
(§) = evalDF

-- XXX: perhaps we could add to VectorSpace a function for projecting vectors
--   (some kind of canonical projection)
(&) :: DiffForm Double -> Vector -> DiffForm Double
(&) = contract dxVP
-- ALSO: generalise Vector? that way we can have parameterised vectors :)
-- kappa, etc. => explicit symbols
-- integration, inner product
