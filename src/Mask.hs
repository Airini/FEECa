{-# LANGUAGE FlexibleContexts #-}
module Mask where

import Spaces
import Polynomials
import Forms
import DiffForms
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

-- dx k n
dx :: Field f => Dim -> Dim -> Form f
dx = oneForm

-- dxN n k
dxN :: Field f => Dim -> Dim -> Form f
dxN = flip dx

(#) :: Form Double -> [Vector] -> Double
(#) = refine dxV
-- TODO: unify
-- complete (##) :: (Field h, VectorSpace v) => Form h -> [v] -> h
--d' :: (Function h v, Algebra (Form h)) => (Int -> v) ->  Monop (Form h)
--d' = df'

canonCoord :: Field a => Int -> Int -> [a]
canonCoord i n = take n $
                  (repeat addId : iterate (addId:) (mulId:repeat addId)) !! i

canonCoords :: Field a => Int -> [[a]]
canonCoords n = map (`canonCoord` n) [1..n]

coordinate :: Field f => Int -> Int -> Polynomial f
coordinate i n = deg1P (canonCoord i n)

coordinates :: Field f => Int -> [Polynomial f]
coordinates = fmap deg1P . canonCoords


bssIx n = vector . flip canonCoord n

-- or <|> ?
(<>) :: Form Double -> Form Double -> Double
(<>) omega eta = inner dxV (bssIx n) omega eta
  where n = dimVec omega

(⌟) :: Form Double -> Vector -> Form Double
(⌟) = contract dxV

interior = (⌟)

𝝹 :: Form (Polynomial Double) -> Form (Polynomial Double)
𝝹 form = contract (const . flip coordinate n) form (undefined::Vector)
  where n = dimVec form
-- TODO: extract degree from polynomial
kappa = 𝝹

-- | Exterior derivative
d :: Monop (Form (Polynomial Double))
d form = df (vector . flip canonCoord n) form
  where n = dimVec form

-- | Evaluation of differential forms at a given point to obtain an alternating form
(§) :: Form (Polynomial Double) -> Point -> Form Double
(§) = evalDF

-- XXX: perhaps we could add to VectorSpace a function for projecting vectors
--   (some kind of canonical projection)
(&) :: DiffForm Double -> Vector -> DiffForm Double
(&) = contract dxVP
-- ALSO: generalise Vector? that way we can have parameterised vectors :)
-- kappa, etc. => explicit symbols
-- integration, inner product
