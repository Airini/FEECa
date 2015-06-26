{-# LANGUAGE FlexibleContexts #-}
module FEEC.Mask where

import Control.Applicative

import FEEC.DifferentialForm
import FEEC.Internal.Form
import FEEC.Internal.Spaces
import FEEC.Internal.Point
import FEEC.Internal.Vector
import FEEC.Polynomial

type Monop t = t -> t
type Binop t = t -> t -> t

(.*) :: VectorSpace v => Scalar v -> v -> v
(.*) = sclV

(.+.) :: VectorSpace v => Binop v
(.+.) = addV

(.+) :: Ring f => Binop f
(.+) = add

º :: Ring f => f
º = addId

(.-) :: Ring f => Monop f
(.-) = addInv

(·) :: Ring f => Binop f
(·) = mul

ı :: Ring f => f
ı = mulId

{- No inverses for rings
(¬) :: Ring f => Monop f
(¬) = mulInv

(÷) :: Ring f => Binop f 
a ÷ b = a · mulInv b  -}

(†) :: Algebra a => Binop a
(†) = addA

(∂) :: Function h v => v -> h -> h
(∂) = deriv

-- dx k n
dx :: Ring f => Dim -> Dim -> Form f
dx = oneForm

-- dxN n k
dxN :: Ring f => Dim -> Dim -> Form f
dxN = flip dx

(#) :: Form Double -> [Vector] -> Double
(#) = refine dxV
-- TODO: unify
-- complete (##) :: (Ringh, VectorSpace v) => Form h -> [v] -> h
--d' :: (Function h v, Algebra (Form h)) => (Int -> v) ->  Monop (Form h)
--d' = df'

canonCoord :: Ring a => Int -> Int -> [a]
canonCoord i n = take n $
                  (repeat addId : iterate (addId:) (mulId:repeat addId)) !! i

canonCoords :: Ring a => Int -> [[a]]
canonCoords n = map (`canonCoord` n) [1..n]

coordinate :: Ring f => Int -> Int -> Polynomial f
coordinate i n = deg1P (canonCoord i n)

coordinates :: Ring f => Int -> [Polynomial f]
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
(§) = eval

-- XXX: perhaps we could add to VectorSpace a function for projecting vectors
--   (some kind of canonical projection)
(&) :: DifferentialForm Double -> Vector -> DifferentialForm Double
(&) = contract dxVP
-- ALSO: generalise Vector? that way we can have parameterised vectors :)
-- kappa, etc. => explicit symbols
-- integration, inner product
