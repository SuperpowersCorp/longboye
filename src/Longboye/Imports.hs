module Longboye.Imports ( interactS ) where

import           Prelude                   hiding ( readFile
                                                  , interact
                                                  , writeFile
                                                  )

import           Data.List                        ( nub
                                                  , sortBy
                                                  )
import           Data.Maybe                       ( fromMaybe )
import           Data.Monoid                      ( (<>) )
import           Data.Ord                         ( comparing )
import           Data.Text                        ( Text )
import qualified Data.Text       as Text
import           Longboye.Import                  ( Import )
import qualified Longboye.Import as Import
import           Longboye.Parser                  ( Parsed( NoImports
                                                          , WithImports
                                                          ) )
import qualified Longboye.Parser as Parser

interactS :: String -> String
interactS contents = Text.unpack $
  case Parser.parseE "<interactive>" (Text.pack contents) of
    Left _                                        -> Text.pack contents
    Right (NoImports s)                           -> s
    Right (WithImports (prefix, imports, suffix)) -> cleanText prefix imports suffix

cleanText :: Text -> [Import] -> Text -> Text
cleanText prefix imports suffix =
  formatPrefix prefix <> formatImports finalImports <> formatSuffix suffix
  where formatPrefix  = (<> "\n\n") . Text.stripEnd
        formatSuffix  = (<> "\n") . Text.stripEnd . (suffixSep <>) . Text.stripStart
        suffixSep     = if (Text.null . Text.strip) suffix then "" else "\n"
        formatImports = Text.unlines . sep . map fmt
        fmt           = Import.format anyQual anyHiding maxModLen maxAsLen
        anyQual       = any Import.qualified imports
        anyHiding     = any Import.hiding imports
        maxModLen     = maximum . map (Text.length . Import.importedModule) $ imports
        maxAsLen      = maximum . map Import.asLength                       $ imports
        finalImports  = nub . sortBy (comparing sortDetails) $ imports
        npo           = length . filter isPreludish $ finalImports
        isPreludish   = flip any ["Prelude", "Overture"] . (==) . Import.importedModule
        sep is        = if npo <= 0
                          then is
                          else mconcat [pos, space, rest]
                            where (pos, rest) = splitAt npo is
                                  space       = [""]
        sortDetails i = fromMaybe (im, q) prioritySortValue
                        where
                          prioritySortValue
                            | im == "Prelude"  = Just ("30", q)
                            | im == "Overture" = Just ("60", q)
                            | otherwise  = Nothing
                          im = Import.importedModule i
                          q  = Import.qualified i
