--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Data.Maybe
import           Text.Blaze.Html5 hiding (map, main)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Core.Item
import qualified Data.Set as S
import           Text.Pandoc.Options
import           System.Process
import           System.FilePath

--------------------------------------------------------------------------------

feecPath :: String
feecPath = "src/FEECa/*.lhs"

extensions              = [ Ext_latex_macros, Ext_literate_haskell ]
mathExtensions          = [ Ext_tex_math_dollars, Ext_tex_math_double_backslash
                          , Ext_latex_macros ]
defaultExtensions       = readerExtensions defaultHakyllReaderOptions
writerDefaultExtensions = writerExtensions defaultHakyllWriterOptions
newExtensions           = Prelude.foldr enableExtension defaultExtensions mathExtensions
newWriterExtensions     = Prelude.foldr enableExtension writerDefaultExtensions (extensions ++ mathExtensions)
readExtensions          = Prelude.foldr enableExtension defaultExtensions (extensions ++ mathExtensions) -- [Ext_literate_haskell]

writerOptions = defaultHakyllWriterOptions {
                  writerExtensions     = newWriterExtensions
                , writerHTMLMathMethod = MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
                }

readerOptions = defaultHakyllReaderOptions {
                  readerExtensions =  readExtensions
                --, readerApplyMacros = True
                }

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  let customExtensions = [ Ext_literate_haskell, Ext_latex_macros ]
      mathExtensions   = [ Ext_tex_math_dollars, Ext_tex_math_double_backslash
                         , Ext_latex_macros ]
  in pandocCompilerWith readerOptions writerOptions

--------------------------------------------------------------------------------

compilePandoc :: Item String -> Item String -> IO (Item String)
compilePandoc macros main = do
    (_,cmdOut,cmdErr) <- readProcessWithExitCode cmd args []
    print cmdErr
    return $ itemSetBody cmdOut main
  where cmd = "pandoc"
        args = [ "-f", "latex+lhs+inline_code_attributes+tex_math_dollars"
               , "--mathjax=https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
--               , "-H", "doc/header",
               , "--bibliography=doc/tech-report.bib"
--               , "--csl=doc/ieee.csl",
               , show (itemIdentifier macros), show (itemIdentifier main) ]

--------------------------------------------------------------------------------

data Menu = Branch String [Menu]
          | Leaf String
          | Empty
  deriving (Show)

getStr :: Menu -> String
getStr (Branch s _) = s
getStr (Leaf s)     = s

addFile :: Menu -> [String] -> Menu
addFile Empty ss = Branch "." [parsePath ss]
addFile (Branch s ms) ss = Branch s (addToMenuList ms ss)
addFile m             _  = m

addToMenuList :: [Menu] -> [String] -> [Menu]
addToMenuList (m:ms) (s:ss)
  | getStr m == s = addFile m ss : ms
  | otherwise     = m : addToMenuList ms (s:ss)
addToMenuList [] ss = [parsePath ss]

parsePath :: [String] -> Menu
parsePath [s]    = Leaf s
parsePath (s:ss) = Branch s [parsePath ss]

showMenu :: Menu -> Int -> Html
showMenu m i = showMenu' [] (skipFolders m i)

showMenu' :: String -> Menu -> Html
showMenu' p (Leaf s) = li (a (toHtml s) ! A.href (toValue $ p ++ "/" ++ s'))
                       ! A.class_ "menu"
  where s' = replaceExtension s ".html"

showMenu' p (Branch s ms) = li (do label (toHtml s)
                                   ul (forM_ ms (showMenu' (p ++ "/" ++ s))) ! A.class_ "menu") ! A.class_ "menu"

skipFolders :: Menu -> Int -> Menu
skipFolders m@(Branch s (m1:ms)) i
  | i < 1     = m
  | otherwise = skipFolders m1 (i-1)

menuCompiler :: Compiler String
menuCompiler = do
  files <- loadAll ("src/**.lhs" .&&. hasVersion "menu") :: Compiler [Item String]
  let p2strs  = splitDirectories . toFilePath . itemIdentifier
      paths   = map p2strs files
      menu    = foldl addFile Empty paths
  return $ relativizeUrlsWith "/src" (renderHtml (showMenu menu 2))

getRoute' :: Identifier -> Compiler FilePath
getRoute' id = do
  route <- getRoute id
  return (fromMaybe  "" route)


--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

         match "web/css/*" $
           route idRoute
           >> compile compressCssCompiler

         match ("src/**.lhs" .&&. hasNoVersion) $ do -- Should we include also .hs files?
           version "menu" $ compile $
             makeItem =<< getRoute' =<< getUnderlying
           route $ setExtension "html"
           compile $ do
             macros <- load "web/macros/macros.sty"
             this <- getResourceString
             unsafeCompiler (compilePandoc macros this)
             >>= loadAndApplyTemplate "web/templates/default.html" menuContext
             >>= relativizeUrls

         match (fromList ["web/index.tex"]) $ do
           version "menu" $ compile $
             makeItem =<< getRoute' =<< getUnderlying
           compile $ pandocCompiler
             >>= loadAndApplyTemplate "web/templates/default.html" menuContext
             >>= relativizeUrls
           route $ constRoute "index.html"

         match "web/templates/*" $ compile templateCompiler

         match "web/macros/macros.sty" $ compile getResourceString
--------------------------------------------------------------------------------

menuContext :: Context String
menuContext =
  field "menu" (const menuCompiler) `mappend`
  defaultContext
