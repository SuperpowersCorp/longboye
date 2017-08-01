module Longboye.SrcSpans
       ( extractPrefix
       , extractSuffix
       , extractImports
       , getImports
       , getModuleStatement
       ) where

import           Data.Text                              ( Text )
import qualified Data.Text                    as Text
import           Language.Haskell.Exts                  ( Module( Module
                                                                , XmlHybrid
                                                                , XmlPage
                                                                )
                                                        , SrcSpanInfo
                                                        , importAnn
                                                        , srcInfoSpan
                                                        , srcSpanEndLine
                                                        , srcSpanStartLine
                                                        )
import           Language.Haskell.Exts.Syntax           ( ImportDecl )
import           Longboye.Import                        ( Import )
import qualified Longboye.Import              as Import
import           Longboye.ModuleStatement               ( ModuleStatement )

getImports :: Module SrcSpanInfo -> [Import]
getImports = map Import.fromDecl <$> extractImports

getModuleStatement :: Module SrcSpanInfo -> Maybe ModuleStatement
getModuleStatement = error "SrcSpans.getModuleStatement not implemented"

extractPrefix :: Module SrcSpanInfo -> Text -> Text
extractPrefix XmlPage {}                        _ = notSupported "XmlPage"
extractPrefix XmlHybrid {}                      _ = notSupported "XmlHybrid"
extractPrefix (Module _ _ _ importDecls _) source =
  Text.unlines . take (n - 1) . Text.lines $ source
  where n = srcSpanStartLine . srcInfoSpan . importAnn . head $ importDecls

extractSuffix :: Module SrcSpanInfo -> Text -> Text
extractSuffix XmlPage {}   _                      = notSupported "XmlPage"
extractSuffix XmlHybrid {} _                      = notSupported "XmlHybrid"
extractSuffix (Module _ _ _ importDecls _) source =
  Text.unlines . drop n . Text.lines $ source
  where n = srcSpanEndLine . srcInfoSpan . importAnn . last $ importDecls

extractImports :: Module SrcSpanInfo -> [ImportDecl SrcSpanInfo]
extractImports (Module _l _ _ decls _) = decls
extractImports XmlHybrid {}            = notSupported "XmlHybrid"
extractImports XmlPage {}              = notSupported "XmlPage"

notSupported :: String -> a
notSupported = error . (++ " modules not supported.")
