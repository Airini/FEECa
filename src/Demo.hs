module Demo where

import Mask
import Spaces
import Polynomials
import Forms
import DiffForms
import PolyN
import Control.Applicative
import Vector
import Point
import Simplex

-------

-- A 2D simplex
x20 = point [0, 0]
x21 = point [1, 0]
x22 = point [0, 1]
t2  = simplex [x20, x21, x22]


-- A 5D simplex
x50 = point [0, 0, 0, 0, 0]
x51 = point [1, 0, 0, 0, 0]
x52 = point [0, 1, 0, 0, 0]
x53 = point [0, 0, 1, 0, 0]
x54 = point [0, 0, 0, 1, 0]
x55 = point [0, 0, 0, 0, 1]
t5  = simplex [x50, x51, x52, x53, x54, x55]

-- Reference simplices
tr1 = referenceSimplex 1
tr2 = referenceSimplex 2
tr3 = referenceSimplex 3
tr4 = referenceSimplex 4
tr5 = referenceSimplex 5

-- Extraction of sub simplices
tr51  = subsimplices t5 1
tr52  = subsimplices t5 2
tr53  = subsimplices t5 3
tr54  = subsimplices t5 4
tr55  = subsimplices t5 5
tr532 = subsimplices (tr53 !! 0) 2


-- v2', v3', v4' :: Vector
v2 = vector [ı,2]
v3 = vector [1,2,3]
v4 = vector [1,2,3,4]

-- Barycentric coordinates - not only on referenceSimplex but also on subsimplices
b1 = barycentricCoords tr1
b2 = barycentricCoords tr2  -- list of length 3 containing polynomials (of degree 1)
b3 = barycentricCoords tr3
b4 = barycentricCoords tr4
b5 = barycentricCoords tr5

-- x :: [PolyN Double]
x = map (deg1 . flip coordinate 2) [1..2]
-- coordinate ==> coordinates
-- hs :: Field a => Int -> [PolyN a]
hs n = pure ı : rec pn1s
  where pn1s = map (deg1 . flip coordinate n) [1..n]
        rec ps = ps ++ (concatMap (\q -> map (q ·) pn1s) ps)


-- p :: PolyN Double
p = 5 .* head x · head x .+. (3 .* head x)
-- TODO: solve precedences
-- TODO: purge null terms
--p = 5 .* (x !! 0) · (x !! 0) .+. 3 .* (x !! 0)

--dxs :: [Form Double]
dxs = map dx [1 .. 5]
dx1 = dxs !! 0
dx2 = dxs !! 1
dx3 = dxs !! 2
dx4 = dxs !! 3
dx5 = dxs !! 4
-- dx n i
w1 = dx1 /\ dx2
w2 = dx3 /\ dx5
w3 = w1  /\ w2

-- val1, val2 :: Double
val1 = w1 # [v2, v2]
val2 = (dx1 /\ dx2) # [vector [1,2], vector [3,4]]

-- dxs' :: [DiffForm Double]
dxs' = map (fmap pure) dxs

w1' = (dxs' !! 0) /\ (dxs' !! 1)
w2' = (dxs' !! 2) /\ (dxs' !! 4)
dx1' = dxs' !! 0
dx2' = dxs' !! 1

w2_aux = (dxs' !! 1) /\ (dxs' !! 0)
-- u :: DiffForm Double
--u = (hs 2 !! 0) .* w1' .+. ((hs 2 !! 3) .* w2') .+. (pure 0.5 .* dx1' /\ dx2')
u = (hs 2 !! 0) .* w1' .+. ((hs 2 !! 3) .* w2_aux) .+. (pure 0.5 .* dx1' /\ dx2')
v = p .* (dxs' !! 0) .+. (2 .* p .* (dxs' !! 1))

-- -- Evaluation of differential forms
val3 = u § x20 # [v2, v2]
val4 = (dx1' /\ dx2') § x22 # [vector [1, 2], vector [3, 4]]
val5 = p .* dx1' § x21 # [v2]

-- Differentiation
du = d 2 u

-- Koszul differential: TODO: remove n dependency (encode in form type)
ku = 𝝹 u 2

-- Inner product
val6 = (w1 <> w2) 5 -- summation over the basis
-- val7 = inner u v t3 -- same summation but also an integration over all x in t3

-- Interior product/contraction
val7 = w1 ⌟ v2
val8 = u & v2
val9 = w1' & v4
val10 = v & v3


