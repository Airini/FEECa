import FEEC.FiniteElementSpace
import FEEC.FiniteElementSpace
import FEEC.Internal.Spaces
import FEEC.Internal.Form
import qualified FEEC.Polynomial as P
import qualified FEEC.Bernstein as B
import qualified FEEC.Internal.Form as F
import FEEC.PolynomialDifferentialForm
import qualified FEEC.Internal.Vector as V
import qualified FEEC.Internal.Simplex as S
import qualified FEEC.Internal.MultiIndex as MI

import System.TimeIt
import Data.List
import Control.DeepSeq
import Debug.Trace

type Family = Int -> Int -> Simplex -> FiniteElementSpace

create_vectors :: Int -> Int -> [Vector]
create_vectors d n = [V.vector (replicate d ((i + 1) `over` (n + 1))) | i <- [0..n]]
    where over i n = divide (fromInt i) (fromInt (n + 1))

faces :: Simplex -> [Int] -> [Simplex]
faces t ks = [S.subsimplex t k 0 | k <- ks]

toString :: Int -> FiniteElementSpace -> [Int] -> [Double] -> [Double] -> [Char]
toString n (PrLk _ k _) rs t1s t2s = "PrLk," ++ show(n) ++ "," ++ show(k)
                                    ++ "," ++ (concat $ intersperse "," (map show rs))
                                    ++ "," ++ (concat $ intersperse "," (map show t1s))
                                    ++ "," ++ (concat $ intersperse "," (map show t2s))
toString n (PrmLk _ k _) rs t1s t2s = "PrmLk," ++ show(n) ++ "," ++ show(k)
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
    rnf (Form k n terms) = rnf terms

--------------------------------------------------------------------------------
-- Benchmark parameters
--------------------------------------------------------------------------------

compute_basis :: Int -> FiniteElementSpace -> IO (Double,[BasisFunction])
compute_basis i s = timeItT (return $!! (replicate i (basis s)) `deepseq` basis s)

evaluate_basis :: [BasisFunction]
               -> Simplex
               -> [Vector]
               -> IO (Double, [Double])
evaluate_basis bs f vs = timeItT (return $!! [evaluate v f | f <- bs', v <- vs])
    where bs' = [apply b (S.spanningVectors f) | b <- bs]


run_benchmark' :: FiniteElementSpace -> Simplex -> [Vector] -> IO (Double,Double)
run_benchmark' s f vs = do (t1,bs) <- compute_basis 1000 s
                           (t2,vs) <- evaluate_basis bs f vs
                           return (t1 / 1000, t2)

run_benchmark :: Family
               -> Int
               -> Int
               -> Simplex
               -> Simplex
               -> [Vector]
               -> IO [Char]
run_benchmark s rmax k t f vs = do let n  = S.topologicalDimension t
                                       rs = [0..rmax]
                                       ss = [s r k t | r <- rs]
                                       rb s = run_benchmark' s f vs
                                   (t1s, t2s) <- fmap unzip (sequence [rb s | s <- ss])
                                   return $ toString n (s 0 k t) rs t1s t2s

benchmarks :: Int
            -> [Family]
            -> Int
            -> Int
            -> IO [Char]
benchmarks nmax families rmax kmax =
    fmap unlines $ sequence $ concat cases
    where cases = [[run_benchmark fam rmax k (ts n) (fs !! k) vs
                        | k <- [0..n]] | n <- [1..nmax], fam <- families]
          vs   = create_vectors nmax 10
          ts n = S.referenceSimplex n
          fs = faces (S.referenceSimplex nmax) [0..nmax]


--------------------------------------------------------------------------------
-- Benchmark main
--------------------------------------------------------------------------------

num_points = 10
num_runs   = 10
max_degree = 5
max_dim    = 4
families   = [PrLk, PrmLk]
filename   = "timings_haskell.csv"

main = do results <- benchmarks max_dim families 10 4
          writeFile "timings_FEEC.csv" results
          putStrLn results

space = PrmLk 3 0 (S.referenceSimplex 2)

