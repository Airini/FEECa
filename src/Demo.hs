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


--v2, v3, v4 :: [Double]
--v2 = [ı,2]
--v3 = [1,2,3]
--v4 = [1,2,3,4]

-- v2', v3', v4' :: Vector
v2 = vector [ı,2]
v3 = vector [1,2,3]
v4 = vector [1,2,3,4]

-- x :: [PolyN Double]
x = map (Poln . deg1P . flip coordinate 2) [1..2]

-- hs :: Field a => Int -> [PolyN a]
hs n = pure ı : rec pn1s
  where pn1s = fmap Poln (map (deg1P . flip coordinate n) [1..n])
        rec ps = ps ++ (concatMap (\q -> map (q ·) pn1s) ps)


-- p :: PolyN Double
p = 5 .* head x · head x .+. (3 .* head x)
-- TODO: solve precedences
--p = 5 .* (x !! 0) · (x !! 0) .+. 3 .* (x !! 0)

--dxs :: [Form Double]
dxs = map dx [0 .. 5]
dx0 = dxs !! 1
dx1 = dxs !! 2
dx2 = dxs !! 3
dx3 = dxs !! 4
dx4 = dxs !! 5
dx5 = dxs !! 6

w1 = dx0 /\ dx1
w2 = dx3 /\ dx5
w3 = w1  /\ w2

-- val1, val2 :: Double
val1 = w1 # [v2, v2]
val2 = (dx0 /\ dx1) # [vector [1,2], vector [3,4]]

-- dxs' :: [DiffForm Double]
dxs' = map (fmap pure) dxs

w1' = (dxs' !! 1) /\ (dxs' !! 2)
w2' = (dxs' !! 3) /\ (dxs' !! 5)
dx1' = dxs' !! 1
dx2' = dxs' !! 2

w2_aux = (dxs' !! 2) /\ (dxs' !! 1)
-- u :: DiffForm Double
--u = (hs 2 !! 0) .* w1' .+. ((hs 2 !! 3) .* w2') .+. (pure 0.5 .* dx1' /\ dx2')
u = (hs 2 !! 0) .* w1' .+. ((hs 2 !! 3) .* w2_aux) .+. (pure 0.5 .* dx1' /\ dx2')
v = p .* (dxs' !! 1) .+. (2 .* p .* (dxs' !! 2))

-- -- Evaluation of differential forms
val3 = u § x20 # [v2, v2]
-- val4 = u[(dx0 /\ dx1) (vector [1, 2]) (vector [3, 4])
val5 = p .* dx1' § x21 # [v2]


du = d 2 u

