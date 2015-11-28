module FEEC.PolynomialDifferentialForms where

import Data.Maybe
import Data.List
import FEEC.Internal.Vector
import FEEC.Internal.Simplex
import qualified FEEC.Bernstein        as B
import qualified FEEC.Internal.Form    as F
import qualified FEEC.DifferentialForm as D
import qualified FEEC.Internal.Spaces  as S


type BernsteinPolynomial a = B.BernsteinPolynomial (Vector a) a
type DifferentialForm a = D.DifferentialForm (BernsteinPolynomial a)

findSimplex :: DifferentialForm a -> Maybe (Simplex (Vector a))
findSimplex (F.Form k n cs) = find (\x -> True) $ mapMaybe (findSimplex' . fst) cs

findSimplex' :: BernsteinPolynomial a -> Maybe (Simplex (Vector a))
findSimplex' (B.Bernstein t _) = Just t
findSimplex' _ = Nothing

apply :: S.Field a => DifferentialForm a -> [Vector a] -> BernsteinPolynomial a
apply omega = F.refine (B.proj t) omega
    where t = fromJust (findSimplex omega)

inner :: S.Field a => DifferentialForm a -> DifferentialForm a -> a
inner omega eta
      | not (null t) = F.inner (B.proj (head t)) omega eta
      | otherwise =
          error "Inner: Need associated simplex to compute inner product."
              where t = catMaybes $ map findSimplex [omega, eta]

