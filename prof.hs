import FEEC.FiniteElementSpace
import FEEC.Internal.Spaces
import FEEC.Internal.Form
import FEEC.Utility.Print
import qualified FEEC.Polynomial as P
import qualified FEEC.Bernstein as B
import qualified FEEC.Internal.Form as F
import qualified FEEC.PolynomialDifferentialForm as D
import qualified FEEC.Internal.Vector as V
import qualified FEEC.Internal.Simplex as S
import qualified FEEC.Internal.MultiIndex as MI

create_vectors :: Int -> Int -> [Vector]
create_vectors d n = [V.vector (replicate d ((i + 1) `over` n)) | i <- [0..n-1]]
    where over i n = divide (fromInt i) (fromInt $ (n + 1) * d)

t     = S.referenceSimplex 3
faces = take 3 (S.subsimplices t 2)
space = PrLk 5 0 t
bs    = basis space
vs    = create_vectors 3 10
fs    = map S.spanningVectors faces
ds    = P.barycentricGradients t

main = print $ sum $ concat $ D.tabulate bs vs faces
