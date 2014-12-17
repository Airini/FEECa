-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Forms where

import Data.List(intersect)
--import Data.Type.Natural
import Spaces
import Discrete

type Dim = Int
data Vector f = Vex Dim [f]
vectorInvariant (Vex n xs) = n == length xs

addList :: Field f => Vector f -> Vector f -> Vector f
addList (Vex n xs) (Vex m ys)
  | n /= m = error "addList: vectors must belong to the same space"
  | otherwise = Vex n (zipWith add xs ys)

scaleList :: Field f => f -> Vector f -> Vector f
scaleList a (Vex n xs) = Vex n (map (mul a) xs)

instance Field f => VectorSpace (Vector f) where
  type Fieldf (Vector f) = f
  vspaceDim (Vex n _) = n
  addV = addList
  sclV = scaleList

instance Show f => Show (Vector f) where
  show (Vex n xs) = show n ++ "-vector " ++ show xs

data Form f =  -- we lose dependency on the type of vector! 
  Fform { arity :: Int -- vecDim??
        , constituents :: [([Int], f)] }
        {-
        , operator :: [v] -> f } -- to be removed? (Can be calculated from constituents -- see refine (unfininshed))
-- where the f in constituents might very well be changed to (v -> f) so as to
-- englobe differential forms
        -}

-- constituents [([1,2],17), ([1,3], 38)] = 17*dx1/\dx2 + 38*dx1/\dx3
v1 :: Vector Double
v1 = Vex 3 [1.0, 2.0, 3.0]
v2 :: Vector Double
v2 = Vex 3 [1.2, 2.5, 1.0]
ex1 = Fform 1 [([1], 2)]

a1 = refine dxV ex1 [v1]
a2 = refine dxV ex2 [v1]
a3 = refine dxV (ex1 //\\ ex2) [v1, v2]
a4 = refine dxV (ex2 //\\ ex1) [v1, v2]

ex2 = Fform 1 [([2], 3)]

t1 = Fform 1 [([2],2.0),([0],-47.0),([2],-35.0),([1],-50.0),([1],-3.0),([0],29.0),([1],-11.0),([1],-17.0),([0],-6.0),([1],30.0)]
t2 = Fform 1 [([2],-17.0),([2],53.0),([0],-36.0),([1],-51.0),([2],-47.0),([1],-28.0),([0],58.0),([0],-48.0),([0],-4.0),([1],20.0)]
b1 = Vex 3 [723.0,255.0,-109.0]
b2 = Vex 3 [-340.0,-1018.0,297.0]
aux1 d1 d2 vs = refine dxV (d1 //\\ d2) vs

t3 = Fform 2 [([2,1],813.0),([1,0],351.0),([1,2],903.0),([3,1],816.0),([2,0],180.0),([0,0],314.0),([0,3],373.0),([0,1],-988.0),([0,3],-284.0),([1,3],301.0),([1,3],-161.0),([0,0],842.0),([0,2],407.0),([1,3],-959.0),([1,3],954.0),([0,1],639.0)]
t4 = Fform 2 [([2,1],981.0),([3,0],150.0),([1,0],692.0),([2,1],674.0),([3,0],-354.0),([3,3],927.0),([1,3],-869.0),([0,3],238.0),([3,1],575.0),([0,3],433.0),([2,0],359.0),([2,1],554.0),([2,1],259.0),([2,3],16.0),([3,0],923.0),([3,3],936.0)]

b3 = Vex 4 [208.0,770.0,-278.0,189.0]
b4 = Vex 4 [601.0,862.0,989.0,-212.0]
b5 = Vex 4 [694.0,669.0,1014.0,-303.0]
b6 = Vex 4 [74.0,268.0,-963.0,-577.0]



constituentsInv :: [([Int],f)] -> Bool
constituentsInv [] = True
constituentsInv ((xs,f):ys) = all (\(xs',_)-> length xs == length xs') ys

-- related to refine
-- evalForm :: Form v f -> [Vector f] -> f
-- evalForm (Fform arity const op) = op 

-- TODO: functor?

instance (Show f) => Show (Form f) where
  show (Fform k cs) = show k ++ "-form: " ++ show cs


dx :: (Field f) => Int -> Form f
dx i | i <= 0    = error "dx: invalid projection of a negative component"
     | otherwise = Fform 1 [([i],mulId)]


-- TODO: missing permutation simplification/identification
(+++) :: (Field f') => Form f' -> Form f' -> Form f'
omega +++ eta
    | arity omega /= arity eta = error "(+++): forms must be of the same dimension"
    | otherwise = Fform (arity eta)
                  (step (constituents omega) (constituents eta))
  where step [] ys = ys
        step xs [] = xs
        step (x:xs) (y:ys)
          | fst x == fst y = (fst x, add (snd x) (snd y)) : step xs ys
          | fst x < fst y  = x : step xs (y:ys)
          | otherwise      = y : step (x:xs) ys

pairM :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairM f h (x,y) = (f x, h y)

(***) :: Field f => f -> Form f -> Form f
a *** omega = Fform (arity omega)
                    (map (pairM id (mul a)) (constituents omega))

(//\\) :: Field f => Form f -> Form f -> Form f
omega //\\ eta = Fform (arity omega + arity eta)
                       (concatMap (\d -> map (combine d) (dxs d)) (constituents eta))
  where dxs (ys,_) = filter (null . intersect ys . fst) (constituents omega)
        combine (ys,b) (xs,a)
          | null (intersect xs ys) = (xs++ys, mul a b)
          | otherwise              = ([],addId)


instance (Field f) => VectorSpace (Form f) where
  type Fieldf (Form f) = f
  vspaceDim _ = undefined
  addV = (+++)
  sclV = (***)

instance (Field f) => Algebra (Form f) where
  addA = addV
  (/\) = (//\\)
  sclA = sclV

-- Our basic projection for 'Vector f'
dxV :: Int -> Vector f -> f
dxV i (Vex n x) = x !! i
--dxV i _   = error "dxV: incorrect number of arguments; must only be 1"

refine :: (Field f, VectorSpace v) =>
          (Int -> v -> f)      -- ^ The definition for the projection function
                               --   for the specific vector space
       -> Form f
       -> [v] -> f
refine proj (Fform k cs) vs = sumF (map (($ vs) . (formify proj)) cs)

-- refine proj (Fform k cs) vs = sumF (map (\(cc,s) -> mul s ((formify proj (cc,s)) vs)) cs) -- ($ vs) . (formify proj)) cs)

-- To be moved
sumF :: Field a => [a] -> a
sumF = foldl add addId
 
-- Sign of a permutation defined by a pair of increasing permutations
sign :: Field f => ([Int], [Int]) -> f
sign (p1, p2) = if (sum [ length (filter (i <) p1) | i <- p2]) `mod` 2 == 0 then mulId else addInv mulId

formify :: (Field f, VectorSpace v) =>
              (i -> v -> f) -> ([i],f) -> [v] -> f
formify proj (i:is, s)
    | null is   = mul s . proj i . head
    | otherwise = \vs ->
        foldl add addId (map (\(w,e) -> mul (mul
                                  (sign (w,e))
                                  ((proj i . head) (choose w vs)))
                                  (formify proj (is,s) (choose e vs)))
                             (permutationPairs (length is + 1) 1 (length is)))
  where choose ns vs = pick (differences ns) vs

-- (vspaceDim (head vs))

zeroForm :: Form f
zeroForm = Fform 0 []

contract :: Form f -> v -> Form f
contract omega | null (constituents omega) = const zeroForm
               | otherwise                 = undefined

-- We need a basis here
(<>) :: Form f -> Form f -> f
omega <> eta = undefined


--- ONLY PLACEHOLDERS!!
data Poly v f = Pp (v -> f)

instance (VectorSpace v, Field f) => Function (Poly v f) v f where
  deriv = undefined
  eval x (Pp g) = g x

instance Field f => VectorSpace (Poly v f) where
  type Fieldf (Poly v f) = f
  vspaceDim = undefined
  addV (Pp g) (Pp h) = Pp $ \vs -> add (g vs) (h vs)
  sclV a (Pp g) = Pp $ \vs -> mul a (g vs)
--



