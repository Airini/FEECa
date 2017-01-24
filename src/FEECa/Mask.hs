{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}


module FEECa.Mask where

-- import Control.Applicative ()

import FEECa.DifferentialForm
import FEECa.Internal.Form
import FEECa.Internal.Spaces hiding (inner)
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

dxVP :: (Eq f, Field f) => Int -> Vector f -> PolyRepresentation f
dxVP = (fmap . fmap) constant dxV

dxVF :: (Eq f, Ring f) => Int -> VectorField f -> PolyRepresentation f
dxVF i (Vector v) = v !! (i-1)

(#) :: forall f v. (Ring f, VectorSpace f,
                    Projectable v (Scalar f), Scalar v ~ Scalar f)
    => Form f -> [v] -> f
(#) = refine projection
-- TODO: unify
-- complete (##) :: (Ring h, VectorSpace v) => Form h -> [v] -> h
--d' :: (Function h v, Algebra (Form h)) => (Int -> v) ->  Monop (Form h)
--d' = df'

class (Ring f, VectorSpace v) => Projectable v f where
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

-- or <|> ?
(<>) :: (Eq f, Field f{-, InnerProductSpace (Form f)-}) => Form f -> Form f -> Scalar f
(<>) omega eta = undefined --S.inner omega eta -- inner dxV omega eta
  where n = dimVec omega
{-
(⌟) :: (Eq f, Field f) => Form f -> Vector f -> Form f
(⌟) = contract dxV
-}

(⌟) :: (Ring f, Projectable v f, Dimensioned v)
     => Form f -> v -> Form f
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
(§) :: DifferentialForm (PolyRepresentation Double) -> Vector Double -> Form Double
(§) = eval

-- XXX: perhaps we could add to VectorSpace a function for projecting vectors
--   (some kind of canonical projection)
(&) :: (Field f, Eq (Vector f)) => DifferentialForm (PolyRepresentation f) -> VectorField f -> DifferentialForm (PolyRepresentation f)
(&) = contract dxVF
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


-- XXX: additional instances for now
instance Functor Vector where
  fmap f (Vector v) = Vector (fmap f v)

instance Functor Term where
  fmap f (Constant a) = Constant (f a)
  fmap f (Term a mi)  = Term (f a) mi

instance Functor Polynomial where
  fmap f (Polynomial n ts) = Polynomial n (fmap (fmap f) ts)

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
