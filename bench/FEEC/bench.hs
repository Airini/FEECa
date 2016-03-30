{-# LANGUAGE FlexibleContexts #-}
module Main where

import FEEC.FiniteElementSpace
import FEEC.Internal.Spaces
import FEEC.Utility.Print
import qualified FEEC.Polynomial    as P
import qualified FEEC.Bernstein     as B
import qualified FEEC.Internal.Form as F
import qualified FEEC.PolynomialDifferentialForm as D
import qualified FEEC.Internal.Vector     as V
import qualified FEEC.Internal.Simplex    as S
import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Utility.Combinatorics
import Criterion
import Criterion.Main

import System.Environment

import System.TimeIt
import Data.List
import Control.DeepSeq
import qualified Control.Exception.Base as C

import Criterion.Main


type Family = Int -> Int -> Simplex -> FiniteElementSpace

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

create_vectors :: Int -> Int -> [Vector]
create_vectors d n = [V.vector (replicate d ((i + 1) `over` n)) | i <- [0..n-1]]
    where over i n = divide (fromInt i) (fromInt $ (n + 1) * d)

evaluate_basis :: [BasisFunction]
               -> [Simplex]
               -> [Vector]
               -> [Double]
evaluate_basis bs fs vs = [applyEval b f v | b <- bs, f <- fs, v <- vs]
  where applyEval b f v = evaluate v (D.apply b (S.spanningVectors f))

--------------------------------------------------------------------------------
-- NFData Instance Declaration
--------------------------------------------------------------------------------

instance NFData a => NFData (P.Term a) where
    rnf (P.Constant c) = rnf c
    rnf (P.Term a mi)  = rnf a `seq` rnf ((MI.toList mi) :: [Int])

instance NFData a => NFData (P.Polynomial a) where
    rnf (P.Polynomial t ts) = rnf ts

instance NFData a => NFData (B.BernsteinPolynomial v a) where
    rnf (B.Bernstein t p) = rnf p
    rnf (B.Constant c)  = rnf c

instance NFData a => NFData (F.Form a) where
    rnf (F.Form k n terms) = rnf terms

--------------------------------------------------------------------------------
-- Benchmark Functions
--------------------------------------------------------------------------------

bench_evaluate :: Family -> [Vector] -> Int -> Int -> Int -> [Benchmark]
bench_evaluate f vs n k rmax = [ bench (show r) $ nf (evaluate_basis (bs r) (faces t)) vs
                                  | r <- [1..rmax] ]
  where t       = S.referenceSimplex n
        faces t = take (n `choose` k) (S.subsimplices t k)
        bs r    = basis (f r k t)

bench_evaluate_n_k :: Family -> Int -> Int -> [Benchmark]
bench_evaluate_n_k f nmax rmax = concat
    [[ bgroup (show (n, k)) $ bench_evaluate f (vs n) n k rmax
        | k <- [0..n] ]
        | n <- [1..nmax] ]
  where vs n = create_vectors n 10

bench_basis_r :: Family -> Int -> Int -> Int -> [Benchmark]
bench_basis_r f n k rmax = [ bench (show r) $ nf (basis . (f r k)) t | r <- [1..rmax] ]
  where t = S.referenceSimplex n

bench_basis_n_k :: Family -> Int -> Int -> [Benchmark]
bench_basis_n_k f nmax rmax = concat
  [[ bgroup (show (n, k)) $ bench_basis_r f n k rmax
      | k <- [0..n] ]
      | n <- [1..nmax] ]

--------------------------------------------------------------------------------
-- Benchmark main
--------------------------------------------------------------------------------

data FEECBenchOptions = FBO
  { max_deg :: Int, max_dim :: Int, max_kar :: Int
  , families :: [Int -> Int -> Simplex -> FiniteElementSpace]
  , npoints :: Int, nruns :: Int
  , filename :: String }

defaultOpts = FBO 5 3 3 [PrLk, PrmLk] 10 10 "timings_FEEC.csv"
{-do
  as <- getArgs >>= return . parseArgs
  let r = max_deg as
      n = max_dim as-}

main = defaultMain
    [ bgroup "PrLk basis"     $ bench_basis_n_k     PrLk n r
    , bgroup "PrLk evaluate"  $ bench_evaluate_n_k  PrLk n r
    , bgroup "PrmLk basis"    $ bench_basis_n_k     PrmLk n r
    , bgroup "PrmLk evaluate" $ bench_evaluate_n_k  PrmLk n r
    ]

parseArgs :: [String] -> FEECBenchOptions
parseArgs ("-f":x:xs) = (parseArgs xs) { filename = x }
parseArgs ("-r":x:xs) = (parseArgs xs) { max_deg  = read x }
parseArgs ("-n":x:xs) = (parseArgs xs) { max_dim  = read x }
parseArgs []          = defaultOpts

--space = PrmLk 3 0 (S.referenceSimplex 2)
