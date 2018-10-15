{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.Imports
  ( cleanText
  ) where

import           Longboye.Prelude

import           Data.List                  ( nub )
import qualified Data.Text        as Text
import           Longboye.Import            ( Import
                                            , members
                                            )
import qualified Longboye.Import  as Import
import           Longboye.Member            ( Member( NamedMember
                                                    , OpMember
                                                    )
                                            )

cleanText :: Text -> [Import] -> Text -> Text
cleanText prefix imports suffix =
  formatPrefix prefix <> formatImports finalImports <> formatSuffix suffix
  where
    formatPrefix  = (<> "\n\n") . Text.stripEnd
    formatSuffix  = (<> "\n") . Text.stripEnd . (suffixSep <>) . Text.stripStart
    suffixSep     = if (Text.null . Text.strip) suffix then "" else "\n"
    formatImports = Text.unlines . sep . map fmt
    fmt           = Import.format anyQual anyHiding maxModLen maxAsLen
    anyQual       = any Import.qualified imports
    anyHiding     = any Import.hiding imports
    maxModLen     = maximum . map (Text.length . Import.importedModule) $ imports
    maxAsLen      = maximum . map Import.asLength                       $ imports
    finalImports  = nub . sortBy (comparing sortDetails) . map sortOps $ imports
    npo           = length . filter isPreludish $ finalImports

    sortOps :: Import -> Import
    sortOps i = i { members = map sortMember <$> members i }
      where
        sortMember m@(NamedMember _ _) = m
        sortMember (OpMember s subs) = OpMember s (sort subs)

    isPreludish = elem "Prelude" . modComponents . Import.importedModule

    modComponents :: Text -> [Text]
    modComponents = Text.splitOn "."

    sep is = if npo <= 0 then is else mconcat [pos, space, rest]
      where
        (pos, rest) = splitAt npo is
        space       = [""]

    sortDetails i = fromMaybe (im, q) prioritySortValue
      where
        prioritySortValue
          | modComponents im == ["Prelude"]     = Just ("30", q)
          | elem "Prelude" . modComponents $ im = Just ("60", q)
          | otherwise                           = Nothing
        im = Import.importedModule i
        q  = Import.qualified i
