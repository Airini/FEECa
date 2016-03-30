{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module FEEC.Internal.Form (
  -- * Generic form types
  Dim, Form (Form, arity, dimVec, terms)

  -- * Predefined primitive constructors
  , zeroForm, nullForm, oneForm

  -- * Form operations
  , refine, refine_basis, inner, contract
  ) where


import Control.Applicative
import Data.List (intersect)
import FEEC.Internal.Spaces hiding( inner )
import qualified FEEC.Internal.Spaces as S( inner )
import FEEC.Utility.Discrete
import FEEC.Utility.Utility (pairM, sumR, sumV, expSign, sign)
import FEEC.Utility.Print (Pretty(..), printForm)
import FEEC.Utility.Combinatorics
import qualified Numeric.LinearAlgebra.HMatrix as M
import qualified Numeric.LinearAlgebra.Data as M

import Debug.Trace

-- * General form: does not depend on the underlying vector space it works on
--   in any way.

type Dim = Int
type Idx = Int
type Prd = [Idx] -- a product of projections / indexed 1-forms
type LinComb f = [(f, Prd)]


-- | Bilinear, alternating forms over vectorspaces
data Form f =  -- we lose dependency on the type of vector!
    Form  { arity   :: Dim            -- ^ For complete evaluation
          , dimVec  :: Dim            -- ^ Of the underlying vector space
          , terms   :: [(f, Prd)] }   -- ^ List of terms of (coeff,wedge)'s
  deriving (Eq, Show)


-- terms [(17, [1,2]), (38, [1,3])] = 17*dx1/\dx2 + 38*dx1/\dx3

-- | Invariant for terms of a defined form: all terms have the same arity
--   ie: each is the result of the exterior product of the same number of 1-forms
termsInv :: [(f, Prd)] -> Bool
termsInv []          = True
termsInv ((_,xs):ys) = all (\(_,xs') -> length xs == length xs') ys

-- NB: because of ((,) t) 's functorial nature, maybe it could make sense to
--   rearrange our terms so as to have them be (inds,coeff) like they used to be?
-- XXX: change so as to inspect result (in case f zeroes out a coeff?)
instance Functor Form where
  fmap f (Form k n cs) = Form k n (map (pairM f id) cs)


instance Pretty f => Pretty (Form f) where
  pPrint (Form k n cs) = printForm "dx" "0" pPrint cs -- show or pPrint...
  {- show k ++ "-form in " ++ show n ++ " dimensions: " ++
                          show (printForm "dx" "0" show cs)-}


-- NB: will be (i -> i -> Ordering) once we normalise all products to be in
--      their normal form - an increasing list
combineWithBy :: (a -> a -> a) -> (i -> i -> Bool)
              -> [(a, i)] -> [(a, i)] -> [(a,i)]
combineWithBy f p es1 es2 = foldl ins es1 es2
  where ins [] x = [x]
        ins (y:ys) x | p (snd x) (snd y) = (fst x `f` fst y, snd y) : ys
                     | otherwise         = y : ins ys x

-- XXX: have combinator
--        aggregate f p cs1 cs2
--      where f :: a -> a -> a (or b -> c)
--            p :: MultiIndex -> MultiIndex -> Bool
--            cs1, cs2 :: [(a, MultiIndex)]
--      such that combines elements from cs2 with those in cs1 (sequentially,
--      no repeat, or filtering those appropriate) with p defining the "quotient"
--      class, f defining the transformation on coeff's
--      OPT: also a MultiIndex transformation? or a separte combinator for it?

-- | Sum of forms
-- Shall we do: permutation simplification/identification
(+++) :: Ring f => Form f -> Form f -> Form f
omega +++ eta
    | degNEq omega eta = errForm "(+++)" BiDegEq
    | spaNEq omega eta = errForm "(+++)" BiSpaEq
    | otherwise = Form (arity eta) (dimVec eta)
                       (step (terms omega) (terms eta))
  where step [] ys = ys
        step xs [] = xs
        step (x:xs) (y:ys)
          | snd x == snd y = let z = add (fst x) (fst y) in
              if fst x /= (addInv . fst) y then (z, snd x) : step xs ys
                            else step xs ys
          | snd x < snd y  = x : step xs (y:ys)
          | otherwise      = y : step (x:xs) ys

-- | Scaling of forms
(***) :: Ring f => f -> Form f -> Form f
(***) a | a == addId = \(Form k n _) -> zeroForm k n
        | otherwise  = fmap (mul a)

-- | (Exterior) Product of forms
(//\\) :: Ring f => Form f -> Form f -> Form f
omega //\\ eta
    | spaNEq omega eta = errForm "(//\\\\)" BiSpaEq
    | otherwise = Form (arity omega + arity eta) (dimVec eta)
                       (concatMap (\d -> map (`combine` d) (dxs d)) (terms eta))
  where dxs (_,ys) = filter (null . intersect ys . snd) (terms omega)
        combine (a,xs) = pairM (mul a) (xs++)

-- | Forms over a 'Ring' form a 'VectorSpace'.
instance Ring f => VectorSpace (Form f) where
  type Scalar (Form f) = f
  addV = (+++)
  sclV = (***)

-- | For 'Form's defined over a 'Ring' we associate an 'Algebra': the exterior
-- algebra.
-- This instance is valid this way since we do not have a restrictive typing
-- for forms and hence addition is blind to arity *type-wise* - however,
-- runtime errors will take place if invalid addition is attempted.
instance Ring f => Algebra (Form f) where
  addA = addV
  (/\) = (//\\)
  sclA = sclV

-- | Basic abstract 1-form
oneForm :: Ring f => Dim -> Dim -> Form f
oneForm i n | i < 0 || i > n = errForm "oneForm" MoProjBd
            | otherwise       = Form 1 n [ (mulId,[i]) ]


-- TODO: shall we have something special for these? no need to state dimension
-- n since they will be constantly zero anyway

-- | The (normalised) == 0 form
zeroForm :: Dim -> Dim -> Form f
zeroForm k n = Form k n []

-- | The k-arity == 0 form
nullForm :: Dim -> f -> Form f
nullForm n f = Form 0 n [(f, [])]


-- Necesitamos una función de pinchado
--  y así pinchar las consecutivas componentes
-- If the function was actually a field, this part would be simplified
contract :: (Ring f, VectorSpace v, Dimensioned v)
         => (Idx -> v -> f) -> Form f -> v -> Form f
contract proj omega v
    | vecNEq omega v = errForm "contract" MoVecEq
    | otherwise      = Form (max 0 (arity omega - 1)) (dimVec omega) $
        concatMap (\c -> map (pinchado c) [1..arity omega])
                  (terms omega)
  where pinchado (f,[]) _ = (f, []) -- error ??
        pinchado (f,ds) i = let (ds1,j:ds2) = splitAt (i-1) ds in
                              (expSign i $ mul f (proj j v), ds1 ++ ds2)
  {- TODO:  optimise
            error handling: proj indexing beyond dimension of v -}

refine_basis :: (Ring r, EuclideanSpace v, Scalar v ~ r)
             => [v] -> [v] -> [r]
refine_basis ds vs = map (fromDouble . M.det) submatrices
  where projections = [[toDouble $ dot d v | v <- vs] | d <- ds]
        submatrices = map ((M.matrix k) . concat) $ kSublists k projections
        k           = length vs

substitute_basis :: (Ring r, VectorSpace v, Scalar v ~ r)
                 => 

-- | Run function for 'Form's: given (an appropriate number of) vector arguments
--   and a 1-form basis (given as a basis-element indexing function 'proj'), it
--   evaluates the form on those arguments
refine :: (Ring w, VectorSpace w, VectorSpace v, Scalar v ~ Scalar w)
       => (Idx -> v -> Scalar w)      -- ^ The definition for the projection function
                                      --   for the specific vector space
       -> Form w
       -> [v] -> w
refine proj eta@(Form k n cs) vs = {-#SCC "Form.refine" #-} sumV (map (($ vs) . formify proj) cs')
  where cs' | null cs   = [(addId,[])]
            | otherwise = cs
-- TODO: capture inconsistency between k and length vs here??
-- ALSO: 0-forms... not evaluating correctly now! Cfr: formify does not accept
--    empty cs
-- XXX: for now proj should take care of the error... change later when settled


-- | Helper function in evaluation: given a 1-form basis, converts a single --   'Form' term into an actual function on vectors
formify :: (Ring w, VectorSpace w, VectorSpace v, Scalar w ~ Scalar v)
        => (i -> v -> Scalar v) -> (w,[i]) -> [v] -> w
formify _    (s, [])   _  = s
formify proj (s, i:is) vs
    | null is   = {-#SCC "Form.formify" #-}sclV (proj i (head vs)) s
    | otherwise =
        foldl addV addId
              (map (\(w,e) -> sclV
                                (mul (sign (w,e)) ((proj i . head) (choose w vs)))
                                (formify proj (s,is) (choose e vs)))
                   (permutationPairs (length is + 1) 1 (length is)))
  where choose ns = pick (differences ns)


-- We need a basis here
inner :: (InnerProductSpace w, EuclideanSpace v, Dimensioned v, Scalar w ~ Scalar v)
      => (Idx -> v -> Scalar w)  -- ^ Projection function in the specific vector space
      -> Form w -> Form w -> Scalar w
inner proj omega eta
    | degNEq omega eta = errForm "inner" BiDegEq -- TODO (??)
    | otherwise = foldl
          (flip $ \vs -> add (S.inner (apply omega vs) (apply eta vs)))
          addId
          (map choose (permutations n (arity omega)))
  where choose is = pick (differences is) (map (unitVector n) [0..n-1])
        apply = refine proj
        n = dimVec omega


-- * Helper functions

-- | Checks arity equality
degNEq :: Form f -> Form f -> Bool
degNEq omega eta = arity omega /= arity eta

-- | Checks combined arity bound
degNBd :: Form f -> Form f -> Bool
degNBd  omega eta = (arity omega + arity eta) <= dimVec omega

-- | Checks compatible underlying vector space dimensions between forms
spaNEq :: Form f -> Form f -> Bool
spaNEq omega eta = dimVec omega /= dimVec eta

-- | Checks compatible underlying vector space dimensions between a form and a
-- 'Dimensioned' type value
vecNEq :: Dimensioned v => Form f -> v -> Bool
vecNEq omega v = dimVec omega /= dim v

errForm :: String -> FormMust -> t
errForm callee obligation = error $ "Form." ++ callee ++
                                    ": forms must " ++ show obligation



-- | Kinds of enforcements to the definitions and operations between/for 'Form'
data FormMust = BiDegEq | BiDegBd | BiSpaEq | MoProjBd | MoVecEq

instance Show FormMust where
  show BiDegEq  = "be of the same degree"
  show BiDegBd  = "have joint degree bounded by the working vector space dimension"
  show BiSpaEq  = "act on the same vector space"
  show MoProjBd = "project components of the underlying vector space"
  show MoVecEq  = "act on vectors of the working vectors space"

