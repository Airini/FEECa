{-# LANGUAGE FlexibleContexts #-}

module FEEC.Utility.GramSchmidt where

import FEEC.Internal.Spaces
import FEEC.Internal.Vector


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
        f u v = divide (dot u v) (dot u u)
gramSchmidt' l _ = l
