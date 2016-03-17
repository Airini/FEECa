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

import System.Environment

import System.TimeIt
import Data.List
import Control.DeepSeq
import qualified Control.Exception.Base as C

import Criterion.Main


type Family = Int -> Int -> Simplex -> FiniteElementSpace

create_vectors :: Int -> Int -> [Vector]
create_vectors d n = [V.vector (replicate d ((i + 1) `over` n)) | i <- [0..n-1]]
    where over i n = divide (fromInt i) (fromInt $ (n + 1) * d)

--faces :: Simplex -> [Int] -> [Simplex]
--faces t ks = [S.subsimplex t k 0 | k <- ks]

toString :: Int -> FiniteElementSpace -> [Int] -> [Double] -> [Double] -> [Char]
toString n (PrLk _ k _) rs t1s t2s = "P," ++ show(n) ++ "," ++ show(k)
                                    ++ "," ++ (concat $ intersperse "," (map show rs))
                                    ++ "," ++ (concat $ intersperse "," (map show t1s))
                                    ++ "," ++ (concat $ intersperse "," (map show t2s))
toString n (PrmLk _ k _) rs t1s t2s = "P-," ++ show(n) ++ "," ++ show(k)
                                    ++ "," ++ (concat $ intersperse "," (map show rs))
                                    ++ "," ++ (concat $ intersperse "," (map show t1s))
                                    ++ "," ++ (concat $ intersperse "," (map show t2s))

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
-- Benchmark parameters
--------------------------------------------------------------------------------

compute_basis :: Int -> FiniteElementSpace -> [IO [BasisFunction]]
compute_basis i s = replicate i (C.evaluate $ force (basis s))

evaluate_basis :: Int
               -> [BasisFunction]
               -> [Simplex]
               -> [Vector]
               -> [IO [Double]]
evaluate_basis i bs fs vs =
  replicate i (do print (sum [applyEval b f v | b <- bs, f <- fs, v <- vs])
                  C.evaluate $ force [applyEval b f v | b <- bs, f <- fs, v <- vs])
    where applyEval b f v = evaluate v (D.apply b (S.spanningVectors f))
          bs' = bs
          normalize b = sclV (B.Constant (1.0 / sqrt (D.inner b b))) b


run_benchmark' :: FiniteElementSpace -> [Simplex] -> [Vector] -> IO (Double,Double)
run_benchmark' s fs vs = do
  (t1, bs) <- timeItT . sequence $ compute_basis 1 s
  (t2,  _) <- timeItT . sequence $ evaluate_basis 1 (head bs) fs vs
  return (t1, t2)

run_benchmark :: Family
              -> Int
              -> Int
              -> Simplex
              -> [Simplex]
              -> [Vector]
              -> IO [Char]
run_benchmark s rmax k t fs vs = do
  let n  = S.topologicalDimension t
      rs = [1..rmax]
      ss = [s r k t | r <- rs]
      rb s = run_benchmark' s fs vs
  (t1s, t2s) <- fmap unzip (sequence [rb s | s <- ss])
  return $ toString n (s 0 k t) rs t1s t2s

benchmarks :: Int
           -> [Family]
           -> Int
--           -> Int       currently not used
           -> IO [Char]
benchmarks nmax families rmax = --kmax =
    (fmap unlines . sequence . concat) cases
  where cases = [ [ run_benchmark fam rmax k (ts n) (fs n k) vs
                    | k <- [0..n] ]
                  | fam <- families, n <- [1..nmax] ]
        vs     = create_vectors nmax 10
        ts n   = S.referenceSimplex n
        fs n k = S.subsimplices (ts n) k


--------------------------------------------------------------------------------
-- Benchmark main
--------------------------------------------------------------------------------

--num_points = 10
--num_runs   = 10
--max_degree = 5
--max_dimens = 3
--families   = [PrLk, PrmLk]
--filename = "timings_FEEC.csv"

data FEECBenchOptions = FBO { max_deg :: Int, max_dim :: Int, max_kar :: Int
                            , families :: [Int -> Int -> Simplex -> FiniteElementSpace]
                            , npoints :: Int, nruns :: Int
                            , filename :: String }

defaultOpts = FBO 5 3 3 [PrLk, PrmLk] 10 10 "timings_FEEC.csv"

main = do as <- getArgs >>= return . parseArgs
          results <- benchmarks (max_dim as) (families as) (max_deg as) -- 3
          writeFile (filename as) results
          putStrLn results

parseArgs :: [String] -> FEECBenchOptions
parseArgs ("-f":x:xs) = (parseArgs xs) { filename = x }
parseArgs ("-r":x:xs) = (parseArgs xs) { max_deg  = read x }
parseArgs ("-n":x:xs) = (parseArgs xs) { max_dim  = read x }
parseArgs []          = defaultOpts

--space = PrmLk 3 0 (S.referenceSimplex 2)
