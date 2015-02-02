module Print where

import Text.PrettyPrint
import Text.Printf
import Spaces

class RenderVector v where
    ncomps :: v -> Int
    components :: v -> [Double]

-- | Print Double with precision p and padded to
-- | width w and precision.
printDouble :: Int -> Int -> Double -> Doc
printDouble w p f = text $ printf "%*.*f" w p f

printComponent :: Int -> Int -> Int -> Int -> Double -> Doc
printComponent w p n i f
    | i == 0 = brNW <> printDouble w p f <> brNE
    | i == n-1=  brSW <> printDouble w p f <> brSE
    | otherwise=  brW <> printDouble w p f <> brE

-- | Print vector using precision p for the components
printVector :: RenderVector v => Int -> v -> Doc
printVector p v
    | n <= 1= space <> text (show comps)
    | otherwise = nest 1 $ vcat (zipWith (printComponent maxw p n) [0..n-1] comps)
    where n = ncomps v
          comps = components v
          maxw = maxWidth p comps

printVectorColl :: RenderVector v => Int -> [v] -> Doc
printVectorColl p vs = vcat $ map hsep [[printComponent (ws!!j) p n i ((ls!!j)!!i)
                                        | j <- [0..m-1]]
                                        | i <- [0..n-1]]
    where ls = map components vs
          ws = map (maxWidth p) ls
          m = length vs
          n = minimum (map ncomps vs)

-- | Compute maximum width w required to print components of the vector
-- | at given precision p.
maxWidth :: Int -> [Double] -> Int
maxWidth p l = (maximum (map (truncate . (logBase 10)) l)) + 4

brNW :: Doc
brNW = text "\9121"

brW :: Doc
brW = text "\9122"

brE :: Doc
brE = text "\9125"

brSW :: Doc
brSW = text "\9123"

brNE :: Doc
brNE = text "\9124"

brSE :: Doc
brSE = text "\9126"

brL :: Doc
brL = text "\x005b"

brR :: Doc
brR = text "\x005d"
