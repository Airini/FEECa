{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Forms (
  Form (Fform, arity)
  , zeroForm, nullForm, oneForm
  , refine
  ) where


import Data.List(intersect)
import Data.Type.Natural
import Spaces
import Discrete
import Utility (pairM)

-- * General form: does not depend on the underlying vector space it works on
--   in any way.

-- | Bilinear, alternating forms over vectorspaces
data Form f =  -- we lose dependency on the type of vector! 
  Fform { arity :: Int                    -- ^ For complete evaluation
        , constituents :: [(f, [Int])] }  -- ^ List of terms of (coeff,wedge)'s

{- where the f in constituents might very well be changed to (v -> f) so as to
   englobe differential forms -}

-- constituents [([1,2],17), ([1,3], 38)] = 17*dx1/\dx2 + 38*dx1/\dx3

-- | Invariant for constituents of a defined form: all terms have the same arity
--   ie: each is the result of the exterior product of the same number of 1-forms
constituentsInv :: [(f, [Int])] -> Bool
constituentsInv []          = True
constituentsInv ((_,xs):ys) = all (\(_,xs') -> length xs == length xs') ys

instance Functor Form where
  fmap f (Fform k cs) = Fform k (map (pairM f id) cs)

instance (Show f) => Show (Form f) where
  show (Fform k cs) = show k ++ "-form: " ++ show cs

-- | Sum of forms
-- Shall we do: permutation simplification/identification
(+++) :: (Field f') => Form f' -> Form f' -> Form f'
omega +++ eta
    | arity omega /= arity eta = error "(+++): forms must be of the same dimension"
    | otherwise = Fform (arity eta)
                  (step (constituents omega) (constituents eta))
  where step [] ys = ys
        step xs [] = xs
        step (x:xs) (y:ys)
          | snd x == snd y = (add (fst x) (fst y), snd x) : step xs ys
          | snd x < snd y  = x : step xs (y:ys)
          | otherwise      = y : step (x:xs) ys

-- | Scaling of forms
(***) :: Field f => f -> Form f -> Form f
a *** omega = Fform (arity omega)
                    (map (pairM (mul a) id) (constituents omega))

-- | Product of forms
(//\\) :: Field f => Form f -> Form f -> Form f
omega //\\ eta = Fform (arity omega + arity eta)
                       (concatMap (\d -> map (combine d) (dxs d)) (constituents eta))
  where dxs (_,ys) = filter (null . intersect ys . snd) (constituents omega)
        combine (b,ys) (a,xs)
          | null (intersect xs ys) = (mul a b, xs++ys)
          | otherwise              = (addId, [])

instance (Field f) => VectorSpace (Form f) where
  type Fieldf (Form f) = f
  vspaceDim _ = undefined
  addV = (+++)
  sclV = (***)

-- This is instance is valid this way since we do not have a restrictive typing
-- for forms and hence addition is blind to arity *type-wise* - however,
-- runtime errors will take place if invalid addition is attempted
instance (Field f) => Algebra (Form f) where
  addA = addV
  (/\) = (//\\)
  sclA = sclV

-- | Basic abstract 1-form
oneForm :: (Field f) => Int -> Form f
oneForm i | i <= 0    = error "oneForm: invalid projection of a negative component"
          | otherwise = Fform 1 [ (mulId,[i]) ]

-- | The (normalised) == 0 form
zeroForm :: Form f
zeroForm = Fform 0 []

-- | The k-arity == 0 form
nullForm :: Int -> Form f
nullForm k = Fform k []

-- | Run function for 'Form's: given (an appropriate number of) vector arguments
--   and a 1-form basis (given as a basis-element indexing function 'proj'), it
--   evaluates the form on those arguments
refine :: (Field f, VectorSpace v) =>
          (Int -> v -> f)      -- ^ The definition for the projection function
                               --   for the specific vector space
       -> Form f
       -> [v] -> f
refine proj (Fform k cs) vs = sumF (map (($ vs) . formify proj) cs)
-- refine proj (Fform k cs) vs = sumF (map (\(cc,s) -> mul s ((formify proj (cc,s)) vs)) cs) -- ($ vs) . (formify proj)) cs)

-- | Helper function in evaluation: given a 1-form basis, converts a single
--   'Form' constituent term into an actual function on vectors
formify :: (Field f, VectorSpace v) =>
              (i -> v -> f) -> (f,[i]) -> [v] -> f
formify proj (s, i:is)
    | null is   = mul s . proj i . head
    | otherwise = \vs ->
        foldl add addId (map (\(w,e) -> mul (mul
                                  (sign (w,e))
                                  ((proj i . head) (choose w vs)))
                                  (formify proj (s,is) (choose e vs)))
                             (permutationPairs (length is + 1) 1 (length is)))
  where choose ns = pick (differences ns)


-- If the function was actually a field, this part would be simplified
contract :: Form f -> v -> Form f
contract omega | null (constituents omega) = const zeroForm
               | otherwise                 = undefined

-- We need a basis here
(<>) :: Form f -> Form f -> f
omega <> eta = undefined


--- ONLY PLACEHOLDERS!!
data Poly v f = Pp (v -> f)

instance (VectorSpace v, f ~ (Fieldf v)) => Function (Poly v f) v where
  type Values (Poly v f) v = f
  deriv = undefined
  eval x (Pp g) = g x

instance Field f => VectorSpace (Poly v f) where
  type Fieldf (Poly v f) = f
  vspaceDim = undefined
  addV (Pp g) (Pp h) = Pp $ \vs -> add (g vs) (h vs)
  sclV a (Pp g) = Pp $ \vs -> mul a (g vs)
--


-- * Helper functions

-- | Sign of a permutation defined by a pair of increasing permutations:
--   specialised to an appropriate 'Field' type (required for operations)
sign :: Field f => ([Int], [Int]) -> f
sign (p1, p2) = if sum [ length (filter (i <) p1) | i <- p2 ] `mod` 2 == 0
                  then mulId
                  else addInv mulId

-- | Equivalent to 'sum' for 'Field' types
-- To be moved
sumF :: Field a => [a] -> a
sumF = foldl add addId
 
