module FEECa.Demo where

-- import Control.Applicative ()

import FEECa.DifferentialForm ( DifferentialForm )
import FEECa.Internal.Form    ( Form, Dim )
import FEECa.Internal.Point
import FEECa.Internal.Simplex
import FEECa.Internal.Spaces
import FEECa.Internal.Vector
import FEECa.Utility.Utility  ( sumR, productR )
import FEECa.Mask
import FEECa.Polynomial

import FEECa.Utility.Print    ( Pretty(..), Doc, text, (<+>) )
import qualified FEECa.Utility.Print as P ( (<>) )
-------

-- TODO: aid, to be removed.
pNotation :: (Pretty a, Show a) => a -> Doc
pNotation x = text (show x) <+> text "~~" <+> pPrint x P.<> text "\n"

-- A 2D simplex
x20, x21, x22 :: Vector Double
x20 = vector [0, 0]
x21 = vector [1, 0]
x22 = vector [0, 1]
t2 :: Simplex (Vector Double)
t2  = simplex [x20, x21, x22]


-- A 5D simplex
x50, x51, x52, x53, x54, x55 :: Vector Double
x50 = vector [0, 0, 0, 0, 0]
x51 = vector [1, 0, 0, 0, 0]
x52 = vector [0, 1, 0, 0, 0]
x53 = vector [0, 0, 1, 0, 0]
x54 = vector [0, 0, 0, 1, 0]
x55 = vector [0, 0, 0, 0, 1]
t5 :: Simplex (Vector Double)
t5  = simplex [x50, x51, x52, x53, x54, x55]

-- Reference simplices
tr1, tr2, tr3, tr4, tr5 :: Simplex (Point Double)
tr1 = referenceSimplex 1
tr2 = referenceSimplex 2
tr3 = referenceSimplex 3
tr4 = referenceSimplex 4
tr5 = referenceSimplex 5

-- Extraction of sub simplices
tr51, tr52, tr53, tr54, tr55, tr532 :: [Simplex (Vector Double)]
tr51  = subsimplices t5 1
tr52  = subsimplices t5 2
tr53  = subsimplices t5 3
tr54  = subsimplices t5 4
tr55  = subsimplices t5 5
tr532 = subsimplices (tr53 !! 0) 2


-- v2', v3', v4' :: Vector
v2 :: Vector Double
v2 = vector [ı,2]
v3, v4 :: Vector Integer
v3 = vector [1,2,3]
v4 = vector [1,2,3,4]

-- Barycentric coordinates - not only on referenceSimplex but also on subsimplices
b1, b2, b3, b4, b5 :: [Polynomial Double]
b1 = barycentricCoordinates tr1
b2 = barycentricCoordinates tr2  -- list of length 3 containing polynomials (of degree 1)
b3 = barycentricCoordinates tr3
b4 = barycentricCoordinates tr4
b5 = barycentricCoordinates tr5

xs :: [PolyRepresentation Double]
xs = coordinates 2

hs :: Ring a => Int -> [Polynomial a]
hs n = constant ı : rec pn1s
  where pn1s = coordinates n
        rec ps = ps ++ concatMap (\q -> map (q ·) pn1s) ps


p :: Polynomial Double
p = constant 5 · x0 · x0 .+. (constant 3 · x0)
  where x0 = head xs

-- TODO: solve precedences
-- TODO: purge null terms


--dxs :: [Form Double]
-- dx for n = 5
-- XXX: dx n k OR dx k n? which should be default (shorter)?
dxs :: Dim -> Form Double
dxs = dxN 5
dx1, dx2, dx3, dx4, dx5 :: Form Double
dx1 = dxs 1
dx2 = dxs 2
dx3 = dxs 3
dx4 = dxs 4
dx5 = dxs 5

dxs2 :: Dim -> Form Double
dxs2 = dxN 2
dx1_2, dx2_2 :: Form Double
dx1_2 = dxs2 1
dx2_2 = dxs2 2

w1, w2, w3, w4 :: Form Double
w1 = dx1 /\ dx2
w2 = dx3 /\ dx5
w3 = w1  /\ w2

w4 = dx1_2 /\ dx2_2

val1, val2 :: Double
val1 = w4 # [v2, v2]
val2 = (dx 1 2 /\ dx 2 2) # ([vector [1,2], vector [3,4]] :: [Vector Double])

dxs', dxs_2' :: Dim -> Form (Polynomial Double)
dxs' = fmap constant . dxs
dxs_2' = fmap constant . dxs2

w1', w2' :: Form (Polynomial Double)
w1' = dxs' 1 /\ dxs' 2
w2' = dxs' 3 /\ dxs' 5

dx1', dx2' :: Form (Polynomial Double)
dx1' = dxs_2' 1
dx2' = dxs_2' 2

w1_2, w2_2 :: Form (Polynomial Double)
w1_2 = dxs_2' 1 /\ dxs_2' 2
w2_2 = dxs_2' 2 /\ dxs_2' 1

u, v :: Form (Polynomial Double)
u = head (hs 2) .* w1_2 .+. ((hs 2 !! 3) .* w2_2) .+. (constant 0.5 .* dx1' /\ dx2')
v = p .* dxs' 1 .+. (constant 2 · p .* dxs' 2)

-- -- Evaluation of differential forms
val3, val4, val5 :: Double
val3 = u § x20 # [v2, v2]
val4 = (dx1' /\ dx2') § x22 # ([vector [1, 2], vector [3, 4]] :: [Vector Double])
val5 = p .* dx1' § x21 # [v2]

-- Differentiation: TODO: control over arity? (bound by dimVec)
du :: Form (PolyRepresentation Double)
du = d u

-- Koszul differential:
ku :: Form (PolyRepresentation Double)
ku = 𝝹 u

-- Inner product
val6 :: Double
val6 = w1 <> w2 -- summation over the basis
-- val7 = inner u v t3 -- same summation but also an integration over all x in t3

-- Interior product/contraction
v5 :: Vector Double
v5 = vector [1..5]
u5 :: Form (Polynomial Double)
u5 = head (hs 2) .* w1' .+. ((hs 2 !! 3) .* w2') .+. (constant 0.5 .* dxs' 1 /\ dxs' 2)
val7, val11 :: Form Double
val7 = w1 ⌟ v5
val8, val9, val10 :: DifferentialForm (PolyRepresentation Double)
val8 = u5 & fmap constant v5
val9 = w1' & fmap constant v5
val10 = v & fmap constant v5
val11 = w4 ⌟ v2
