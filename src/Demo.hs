{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Spaces
import Polynomials
import Forms hiding (Vector, dxV)
import DiffForms
import PolyN
import Control.Applicative
import Vector

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

d' :: (Function h v, Algebra (Form h)) => (Int -> v) ->  Monop (Form h)
d' = flip df' undefined

coordinate :: Field a => Int -> Int -> [a]
coordinate i n = replicate (i-1) addId ++ mulId: replicate (n-i) addId

hs n = pure mulId : rec pn1s
  where pn1s = fmap Poln (map (deg1P . flip coordinate n) [1..n])
        rec ps = ps ++ (concatMap (\q -> map (q ·) pn1s) ps)

-------

v2, v3, v4 :: [Double]
v2 = [ı,2]
v3 = [1,2,3]
v4 = [1,2,3,4]

v2', v3', v4' :: Vector
v2' = vector [ı,2]
v3' = vector [1,2,3]
v4' = vector [1,2,3,4]

x :: [PolyN Double]
x = map (Poln . deg1P . flip coordinate 2) [1..2]

p :: PolyN Double
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

(#) :: Form Double -> [Vector] -> Double
(#) = refine dxV

val1, val2 :: Double
val1 = w1 # [v2', v2']
val2 = (dx0 /\ dx1) # [vector [1,2], vector [3,4]]

dxs' :: [DiffForm Double]
dxs' = map (fmap pure) dxs

w1' = (dxs' !! 1) /\ (dxs' !! 2)
w2' = (dxs' !! 3) /\ (dxs' !! 5)
dx1' = dxs' !! 2
dx2' = dxs' !! 3

u :: DiffForm Double
u = (hs 2 !! 0) .* w1' .+. ((hs 2 !! 3) .* w2') .+. (pure 0.5 .* dx1' /\ dx2')
v = p .* (dxs' !! 1) .+. (2 .* p .* (dxs' !! 2))

d = df' (vector . flip coordinate 2) (vector $ coordinate 0 2)
du = d u
