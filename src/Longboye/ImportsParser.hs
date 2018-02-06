{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.ImportsParser
  ( parse
  , parseE
  ) where

import qualified Prelude                         as P
import           Longboye.Prelude

import qualified Data.Text                       as Text
import           Language.Haskell.Exts                     ( Module( Module
                                                                   , XmlHybrid
                                                                   , XmlPage
                                                                   )
                                                           , SrcSpanInfo
                                                           , importAnn
                                                           , parseFileContentsWithMode
                                                           , srcInfoSpan
                                                           , srcSpanEndLine
                                                           , srcSpanStartLine
                                                           )
import           Language.Haskell.Exts.Extension           ( Extension
                                                           , Language( Haskell2010 )
                                                           )
import           Language.Haskell.Exts.Parser              ( ParseResult( ParseFailed
                                                                        , ParseOk
                                                                        )
                                                           , baseLanguage
                                                           , defaultParseMode
                                                           , extensions
                                                           , ignoreLanguagePragmas
                                                           , parseFilename
                                                           )
import           Language.Haskell.Exts.Syntax              ( ImportDecl )
import           Longboye.Files                            ( Contents( WithSubject
                                                                     , WithoutSubject
                                                                     )
                                                           )
import           Longboye.Import                           ( Import )
import qualified Longboye.Import                 as Import

parse :: [Extension] -> FilePath -> Text -> Maybe (Contents [Import])
parse foundExtensions path = eitherToMaybe . parseE foundExtensions path

parseE :: [Extension] -> FilePath -> Text -> Either Text (Contents [Import])
parseE foundExtensions path source = case parseFileContentsWithMode parseMode sourceText of
  ParseOk parsedMod ->
    if null imports
      then Right . WithoutSubject $ source
      else Right . WithSubject $ (prefix, imports, suffix)
    where
      imports = getImports parsedMod
      prefix  = extractPrefix parsedMod source
      suffix  = extractSuffix parsedMod source
  ParseFailed srcLoc' err ->
    Left $ "ERROR at " <> show srcLoc' <> ": " <> show err
  where
    parseMode = defaultParseMode
      { baseLanguage          = Haskell2010
      , ignoreLanguagePragmas = False
      , extensions            = configuredExtensions
      , parseFilename         = path
      }
    configuredExtensions = extensions defaultParseMode ++ foundExtensions
    sourceText           = Text.unpack source

extractPrefix :: Module SrcSpanInfo -> Text -> Text
extractPrefix XmlPage {}                        _ = notSupported "XmlPage"
extractPrefix XmlHybrid {}                      _ = notSupported "XmlHybrid"
extractPrefix (Module _ _ _ importDecls _) source =
  Text.unlines . take (n - 1) . Text.lines $ source
  where
    n = srcSpanStartLine . srcInfoSpan . importAnn . P.head $ importDecls

extractSuffix :: Module SrcSpanInfo -> Text -> Text
extractSuffix XmlPage {}   _                      = notSupported "XmlPage"
extractSuffix XmlHybrid {} _                      = notSupported "XmlHybrid"
extractSuffix (Module _ _ _ importDecls _) source =
  Text.unlines . drop n . Text.lines $ source
  where
    n = srcSpanEndLine . srcInfoSpan . importAnn . P.last $ importDecls

extractImports :: Module SrcSpanInfo -> [ImportDecl SrcSpanInfo]
extractImports (Module _l _ _ decls _) = decls
extractImports XmlHybrid {}            = notSupported "XmlHybrid"
extractImports XmlPage {}              = notSupported "XmlPage"

getImports :: Module SrcSpanInfo -> [Import]
getImports = map Import.fromDecl <$> extractImports
