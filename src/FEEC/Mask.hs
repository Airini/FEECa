{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module FEEC.Mask where

import Control.Applicative

import FEEC.DifferentialForm
import FEEC.Internal.Form
import FEEC.Internal.Spaces hiding(inner)
import qualified FEEC.Internal.Spaces as S(inner)
import FEEC.Internal.Vector
import FEEC.Internal.Simplex
import FEEC.Polynomial
import FEEC.Utility.Utility

type Monop t = t -> t
type Binop t = t -> t -> t
type PolyRepresentation = Polynomial 

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
(∂) = derive

-- dx k n
dx :: Ring f => Dim -> Dim -> Form f
dx = oneForm

-- dxN n k
dxN :: Ring f => Dim -> Dim -> Form f
dxN = flip dx

dxVP :: (Eq (Vector f), Field f) => Int -> Vector f -> PolyRepresentation f
dxVP = (fmap . fmap) constant dxV

(#) :: Form Double -> [Vector Double] -> Double
(#) = undefined -- refine dxV
-- TODO: unify
-- complete (##) :: (Ringh, VectorSpace v) => Form h -> [v] -> h
--d' :: (Function h v, Algebra (Form h)) => (Int -> v) ->  Monop (Form h)
--d' = df'

canonCoord :: Ring a => Int -> Int -> [a]
canonCoord i n = take n $
                  (repeat addId : iterate (addId:) (mulId:repeat addId)) !! i

canonCoords :: Ring a => Int -> [[a]]
canonCoords n = map (`canonCoord` n) [1..n]

coordinate :: Ring f => Int -> Int -> PolyRepresentation f
coordinate i n = linearPolynomial (canonCoord i n)

coordinates :: Ring f => Int -> [PolyRepresentation f]
coordinates = fmap linearPolynomial . canonCoords


bssIx n = vector . flip canonCoord n

-- or <|> ?
(<>) :: (Eq (Vector f), Field f) => Form f -> Form f -> f
(<>) omega eta = undefined -- inner dxV omega eta
  where n = dimVec omega

(⌟) :: (Eq (Vector f), Field f) => Form f -> Vector f -> Form f 
(⌟) = contract dxV

interior :: (Eq (Vector f), Field f) => Form f -> Vector f -> Form f 
interior = (⌟)

𝝹 :: Form (PolyRepresentation Double) -> Form (PolyRepresentation Double)
𝝹 form = contract (const . flip coordinate n) form (undefined::Vector Double)
  where n = dimVec form
-- TODO: extract degree from polynomial
kappa = 𝝹

-- | Exterior derivative
d :: Monop (Form (PolyRepresentation Double))
d form = df (vector . flip canonCoord n) form
  where n = dimVec form

-- | Evaluation of differential forms at a given point to obtain an alternating form
(§) :: DifferentialForm (PolyRepresentation Double) -> Vector Double -> Form Double
(§) = eval

-- XXX: perhaps we could add to VectorSpace a function for projecting vectors
--   (some kind of canonical projection)
(&) :: (Field f, Eq (Vector f)) => DifferentialForm (PolyRepresentation f) -> Vector f -> DifferentialForm (PolyRepresentation f)
(&) = contract dxVP
-- ALSO: generalise Vector? that way we can have parameterised vectors :)
-- kappa, etc. => explicit symbols
-- integration, inner product


--integral :: (FiniteElement t r, Function f (Primitive) =>
integral :: (EuclideanSpace v, f ~ Scalar v) => Simplex v -> DifferentialForm (PolyRepresentation f) -> f
integral t f = integratePolynomial t undefined {- (f#vs)
  where vs = fmap (lii ) $ spanningVectors t
        lii v = vector (fmap constant (toList v))-}

{-
instance Field f => Field (PolyRepresentation f) where
  mulInv = undefined
  fromDouble = constant . fromDouble
  toDouble = undefined
-}

