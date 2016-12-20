{-# LANGUAGE FlexibleContexts #-}

module FEECa.Utility.GramSchmidt where

import FEECa.Internal.Spaces


-- | Gram-Schmidt orthogonalization
gramSchmidt :: (EuclideanSpace v, Eq (Scalar v))
            => [v]
            -> [v]
gramSchmidt = reverse . gramSchmidt' []

gramSchmidt' :: (EuclideanSpace v, Eq (Scalar v))
             => [v]
             -> [v]
             -> [v]
gramSchmidt' l (v:vs)
    | dot v' v' == addId = gramSchmidt' l vs
    | otherwise = gramSchmidt' (v':l) vs
  where v' = foldl subV v [sclV (f u v) u | u <- l, dot u u /= addId]
        f u w = divide (dot u w) (dot u u)
gramSchmidt' l _ = l
