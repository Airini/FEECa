{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
-- XXX: {-# LANGUAGE UndecidableInstances   #-}


module FEECa.Mask where

-- import Control.Applicative ()

import FEECa.DifferentialForm
import FEECa.Internal.Form hiding ( inner )
import qualified FEECa.Internal.Form as F ( inner )
import FEECa.Internal.Spaces
import FEECa.Internal.Vector
import FEECa.Internal.Simplex
import FEECa.Polynomial
import qualified FEECa.Internal.MultiIndex as MI


type Monop t = t -> t
type Binop t = t -> t -> t
type PolyRepresentation = Polynomial
type VectorField a = Vector (PolyRepresentation a)

tangential :: Ring a => Int -> VectorField a
tangential n = vector (map (monomial . MI.unit n) [0..n-1])

(.*) :: Module v => Scalar v -> v -> v
(.*) = sclV

(.+.) :: Module v => Binop v
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

(¬) :: Field f => Monop f
(¬) = mulInv

(÷) :: Field f => Binop f
a ÷ b = a · mulInv b

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

dxVP :: Field f => Int -> Vector f -> PolyRepresentation f
dxVP = (fmap . fmap) constant dxV

dxVF :: Ring f => Int -> VectorField f -> PolyRepresentation f
dxVF i (Vector v) = v !! (i-1)

(#) :: forall f v. (Ring f, Module f,
                    Projectable v (Scalar f), Scalar v ~ Scalar f)
    => Form f -> [v] -> f
(#) = refine projection
-- TODO: unify
-- complete (##) :: (Ring h, VectorSpace v) => Form h -> [v] -> h
--d' :: (Function h v, Algebra (Form h)) => (Int -> v) ->  Monop (Form h)
--d' = df'

-- TODO: check constraints
class (Ring f, Module v) => Projectable v f where
  projection :: Int -> v -> f

instance Field f => Projectable (Vector f) f where
  projection = dxV
instance Field f => Projectable (Vector f) (PolyRepresentation f) where
  projection = (fmap . fmap) constant dxV
instance Ring f => Projectable (VectorField f) (PolyRepresentation f) where
  projection i (Vector v) = v !! (i-1)

canonCoord :: Ring a => Int -> Int -> [a]
canonCoord i n = take n $
                  (repeat addId : iterate (addId:) (mulId:repeat addId)) !! i

canonCoords :: Ring a => Int -> [[a]]
canonCoords n = map (`canonCoord` n) [1..n]

coordinate :: Ring f => Int -> Int -> PolyRepresentation f
coordinate i n = linearPolynomial (canonCoord i n)

coordinates :: Ring f => Int -> [PolyRepresentation f]
coordinates = fmap linearPolynomial . canonCoords

-- TODO: used? or just for testing code?
bssIx :: Ring a => Int -> Int -> Vector a
bssIx n = vector . flip canonCoord n

{- XXX: TODO: quick, dirty fix for now. REDO!
instance (Ring a, VectorSpace a, a ~ Scalar a) => InnerProductSpace a where
  inner = mul
-}
instance InnerProductSpace Double where
  inner = mul

-- or <|> ?
(<>) :: (Field f, InnerProductSpace f)
        {-, Projectable (Vector (Scalar f)) f) -}
     => Form f -> Form f -> Scalar f
(<>) omega eta = F.inner dxV omega eta -- undefined --S.inner omega eta -- inner dxV omega eta
  where n = dimVec omega
{-
(⌟) :: (Eq f, Field f) => Form f -> Vector f -> Form f
(⌟) = contract dxV
-}

(⌟) :: (Projectable v f, Dimensioned v) => Form f -> v -> Form f
(⌟) = contract projection

{-interior :: (Eq f, Field f) => Form f -> Vector f -> Form f
interior = (⌟)-}

𝝹 :: Ring f => Form (PolyRepresentation f) -> Form (PolyRepresentation f)
𝝹 form = contract dxVF form (tangential n) --(const . flip coordinate n)
  where n = dimVec form
-- TODO: extract degree from polynomial

kappa :: Ring f => Form (PolyRepresentation f) -> Form (PolyRepresentation f)
kappa = 𝝹

-- | Exterior derivative
d :: Monop (Form (PolyRepresentation Double))
d form = df (vector . flip canonCoord n) form
  where n = dimVec form

-- | Evaluation of differential forms at a given point to obtain an alternating form
(§) :: DifferentialForm (PolyRepresentation Double) -> Vector Double
    -> Form Double
(§) = eval

-- XXX: perhaps we could add to VectorSpace a function for projecting vectors
--   (some kind of canonical projection)
(&) :: Field f
    => DifferentialForm (PolyRepresentation f) -> VectorField f
    -> DifferentialForm (PolyRepresentation f)
(&) = contract dxVF
-- ALSO: generalise Vector? that way we can have parameterised vectors :)
-- kappa, etc. => explicit symbols
-- integration, inner product


--integral :: (FiniteElement t r, Function f (Primitive) =>
integral :: (EuclideanSpace v, f ~ Scalar v) => Simplex v -> DifferentialForm (PolyRepresentation f) -> f
integral t _f = integratePolynomial t undefined {- (f#vs)
  where vs = fmap (lii ) $ spanningVectors t
        lii v = vector (fmap constant (toList v))-}

{-
instance Field f => Field (PolyRepresentation f) where
  mulInv = undefined
  fromDouble = constant . fromDouble
  toDouble = undefined
-}

{-
instance Applicative Polynomial where
  pure = constant
  (<*>) = undefined
-}

{-
instance (Field r, EuclideanSpace (Vector r), r ~ Scalar (Vector r))  =>
    InnerProductSpace (Polynomial r) where
  inner :: forall r. Polynomial r -> Polynomial r -> r
  inner p1 p2 = integratePolynomial (referenceSimplex 4 :: Simplex (Vector r)) (mul p1 p2)
-}
