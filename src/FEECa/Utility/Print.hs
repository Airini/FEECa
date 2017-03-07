module FEECa.Utility.Print (
    Pretty (..)
  , printDouble, printComponent, printVector, printVectorRow
  , printBernstein, printPolynomial, printPolynomial0, printForm
  , lambda, dlambda, (<>), (P.$$), (P.$+$), text, rn, int
  ) where

import            Text.PrettyPrint
import qualified  Text.PrettyPrint.HughesPJ as P
import            Text.Printf
import qualified  FEECa.Internal.MultiIndex as MI
import            Data.List (intersperse)


-- Some symbols
dlambda, lambda, phi :: String
dlambda = 'd':lambda
lambda  = "\x03BB"
phi     = "\x03D5"

-- class RenderVector v where
--    ncomps :: v -> Int
--    components :: v -> [Double]

-- | Pretty class prtotype.
class Pretty p where
    pPrint :: p -> Doc

-- | Pretty printing for lists of Pretty instances.
instance (Pretty a) => Pretty [a] where
    pPrint [] = text "Empty List"
    pPrint l = text "[ " P.$$ foldl addline empty l P.<+> text "]"
        where addline x y = (x <> comma) P.$+$ pPrint y

-- | Render the symbol for Euclidean space of dimension n.
rn :: Int -> Doc
rn n = text "\x211D" <> printSuperscript n

-- | Print Double with precision p and padded to
-- | width w and precision.
printDouble :: Int -> Int -> Double -> Doc
printDouble w p f = text $ printf "%*.*f" w p f
-- TODO: caputre 1's, 0's, (-) here?

-- TODO: p is precision for float?

printComponent :: Int -> Int -> Int -> Int -> Double -> Doc
printComponent w p n i f
    | i == 0 = brNW <> printDouble w p f <> brNE
    | i == n-1=  brSW <> printDouble w p f <> brSE
    | otherwise=  brW <> printDouble w p f <> brE

-- | Print vector using precision p for the components
printVector :: Int -> [Double] -> Doc
printVector p cs
    | n <= 1= space <> text (show cs)
    | otherwise = nest 1 $ vcat (zipWith (printComponent maxw p n) [0..n-1] cs)
    where n = length cs
          maxw = maxWidth p cs

printVectorRow :: Int -> [[Double]] -> Doc
printVectorRow p ls = vcat $ map hsep [[printComponent (ws!!j) p n i ((ls!!j)!!i)
                                        | j <- [0..m-1]]
                                        | i <- [0..n-1]]
    where ws = map (maxWidth p) ls
          m = length ls
          n = minimum (map length ls)

-- | Compute maximum width w required to print components of the vector
-- | at given precision p.
maxWidth :: Int -> [Double] -> Int
maxWidth p l = maximum (map numLen l) + p + 1
    where numLen n
              | n < 0.0   = truncate (logBase 10 n) + 2
              | otherwise = truncate (logBase 10 n) + 1


-- | Pretty print polynomial
printPolynomial :: [Char] -> [(Double,MI.MultiIndex)] -> Doc
printPolynomial _   []           = double 0.0
printPolynomial sym [ (c,mon) ]  = double c <+> printMonomial sym (MI.toList mon)
printPolynomial sym ((c,mon):ls) = s <+> text "+" <+> printPolynomial sym ls
    where s = double c <+> printMonomial sym (MI.toList mon)

-- | Pretty print polynomial
printPolynomial0 :: [Char] -> [(Double,MI.MultiIndex)] -> Doc
printPolynomial0 _   []           = double 0.0
printPolynomial0 sym [ (c,mon) ]  = double c <+> printMonomial0 sym (MI.toList mon)
printPolynomial0 sym ((c,mon):ls) = s <+> text "+" <+> printPolynomial sym ls
    where s = double c <+> printMonomial0 sym (MI.toList mon)

-- | Pretty print polynomial
printBernstein :: [(Double,MI.MultiIndex)] -> Doc
printBernstein []           = double 0.0
printBernstein [(c,mon)]  = double c <+> printMonomial0 lambda (MI.toList mon)
printBernstein ((c,mon):ls) = if c == 0
                              then printPolynomial lambda ls
                              else s <+> text "+" <+> printPolynomial lambda ls
    where s = double c <+> printMonomial0 lambda (MI.toList mon)


-- | Pretty print constant
printConstant :: Double -> Doc
printConstant = double

-- | Pretty print monomial using sym for the components
printMonomial :: [Char] -> [Int] -> Doc
printMonomial sym = printMonomial' sym 1

-- | Pretty print monomial using sym for the components
printMonomial0 :: [Char] -> [Int] -> Doc
printMonomial0 sym = printMonomial' sym 0

printMonomial' :: Integral a => [Char] -> a -> [a] -> Doc
printMonomial' sym i (l:ls)
    | l > 0     = s <> printMonomial' sym (i+1) ls
    | otherwise = printMonomial' sym (i+1) ls
  where s = text sym <> printSub i <> printPower l
printMonomial' _ _ [] = baseD

-- | Print symbol for PrLambdak space
printPrLambdak :: Int -> Int -> Doc
printPrLambdak r k = text "\x1D4DF" <> printSub r <> text "\x039B" <> printSuperscript k

-- | Print symbol for PrMinusLambdak space
printPrMinusLambdak :: Int -> Int -> Doc
printPrMinusLambdak r k = text "\x1D4DF" <> text "\x207B" <> printSub r <> text "\x039B" <> printSuperscript k

-- | Print Whitney form
-- TODO: Print indices > 10 with brackets
printWhitneyForm :: (f -> [Char]) -> [(f,[Int])] -> Doc
printWhitneyForm p ls = hsep $ punctuate (text " + ") (map printPair ls)
  where
    printPair (a,b) = lparen <> text (p a) <> rparen <+> printPhi b
    printPhi ls' = text phi <> hcat (map printSub ls')

-- OR: unit as f + apply coeff
printForm :: [Char] -> [Char] -> (f -> Doc) -> [(f,[Int])] -> Doc
printForm _  unit _     []   = text unit -- $ coeff addId
printForm df _    coeff rest = hsep $ punctuate (text " +") $
  map (\(a,cs) -> text "(" <> coeff a <> text ")" <+>
                  hsep (intersperse wedgeD (map ((<>) (text df) . printSub) cs)))
      rest

-- * Auxiliary prints

baseD :: Doc
baseD = text ""

-- | Pretty print exponent using unicode superscripts. Prints "" for
-- | 0.
printPower :: Integral a => a -> Doc
printPower i
    | i==2  = text "\x00B2"
    | i==3  = text "\x00B3"
    | i==4  = text "\x2074"
    | i==5  = text "\x2075"
    | i==6  = text "\x2076"
    | i==7  = text "\x2077"
    | i==8  = text "\x2078"
    | i==9  = text "\x2079"
    | i > 9 = printPower (div i 10)  <> printPower (mod i 10)
    | otherwise = text ""
  -- where ld = truncate (logBase 10 (fromIntegral i))

printSuperscript :: Integral a => a -> Doc
printSuperscript i
    | i==1      = text "\x00B9"
    | otherwise = printPower i

-- | Pretty print subscript using unicode subscripts.
printSub :: Integral a => a -> Doc
printSub i
    | i==0  = text "\x2080"
    | i==1  = text "\x2081"
    | i==2  = text "\x2082"
    | i==3  = text "\x2083"
    | i==4  = text "\x2084"
    | i==5  = text "\x2085"
    | i==6  = text "\x2086"
    | i==7  = text "\x2087"
    | i==8  = text "\x2088"
    | i==9  = text "\x2089"
    | i > 9 = printSub (div i 10)  <> printSub (mod i 10)
    | otherwise = text ""
  -- where ld = truncate (logBase 10 (fromIntegral i))

wedgeD :: Doc
wedgeD = text "\x2227"

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
