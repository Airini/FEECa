{-# LANGUAGE FlexibleContexts #-}

module FEECa.Utility.GramSchmidt where

import FEECa.Internal.Spaces


-- | Gram-Schmidt orthogonalization
gramSchmidt :: (EuclideanSpace v, Eq (Scalar v))
            => [v]
            -> [v]
gramSchmidt [] = []
gramSchmidt vs = reverse $ gramSchmidt' (dim $ head vs) [] vs
-- TODO: remove reverse!!!

gramSchmidt' :: (EuclideanSpace v, Eq (Scalar v))
             => Int
             -> [v]
             -> [v]
             -> [v]
gramSchmidt' n l (v:vs)
    | n == 0              = l
    | dot v' v' == addId  = gramSchmidt' n l vs
    | otherwise           = gramSchmidt' (n-1) (v':l) vs
  where v'    = foldl subV v [sclV (f u v) u | u <- l]
              -- TODO: check tests, but (dot u u /= addId) as a side-condition
              --  in the list comprehension must be redundant due to the guard
        f u w = divide (dot u w) (dot u u)
gramSchmidt' _ l _        = l

