module GramSchmidt where

import Vector
import Spaces

-- | Gram-Schmidt orthogonalization
gramSchmidt :: [Vector] -> [Vector]
gramSchmidt = reverse . (gramSchmidt' [])

gramSchmidt' :: [Vector] -> [Vector] -> [Vector]
gramSchmidt' l (v:vs)
    | (dot v' v') == 0 = gramSchmidt' l vs
    | otherwise = gramSchmidt' (v':l) vs
    where v' = foldl subV v [sclV ((dot u v)/(dot u u)) u | u <- l, (dot u u) /= 0]
gramSchmidt' l _ = l
