{-# LANGUAGE FlexibleContexts #-}

module FEECa.Utility.GramSchmidt (

    gramSchmidt, extendOrthGS

  ) where

import FEECa.Internal.Spaces


-- | Gram-Schmidt orthogonalization
gramSchmidt :: EuclideanSpace v
            => [v]
            -> [v]
gramSchmidt [] = []
gramSchmidt vs = gramSchmidt' (dim $ head vs) [] vs

extendOrthGS :: EuclideanSpace v
             => Either [v] Int
             -> [v]
extendOrthGS t  = vs ++ gramSchmidt' (n-k) vs (map (unitVector n) [0..n-1])
  where (n, vs) = case t of
                    Right d -> (d            , [])
                    Left os -> (dim $ head os, os)
        k       = length vs

gramSchmidt' :: EuclideanSpace v
             => Int
             -> [v]
             -> [v]
             -> [v]
gramSchmidt' _ _  []      = []
gramSchmidt' n us (v:vs)
    | n == 0              = []
    | dot v' v' == addId  = gramSchmidt' n us vs
    | otherwise           = v' : gramSchmidt' (n-1) (v':us) vs
  where v'      = foldl subV v [sclV (f u v) u | u <- us]
        f u w   = divide (dot u w) (dot u u)

