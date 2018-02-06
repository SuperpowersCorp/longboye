{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.PragmasParser
  ( Pragma
  , parse
  , parseE
  ) where

import qualified Prelude
import           Longboye.Prelude

import qualified Data.Text                       as Text
import           Language.Haskell.Exts                   ( Module( Module
                                                                 , XmlHybrid
                                                                 , XmlPage
                                                                 )
                                                         , ModulePragma
                                                         , SrcSpanInfo
                                                         , ann
                                                         , parseFileContentsWithMode
                                                         , srcInfoSpan
                                                         , srcSpanEndLine
                                                         , srcSpanStartLine
                                                         )
import           Language.Haskell.Exts.Extension         ( Extension
                                                         , Language( Haskell2010 )
                                                         )
import           Language.Haskell.Exts.Parser            ( ParseResult( ParseFailed
                                                                      , ParseOk
                                                                      )
                                                         , baseLanguage
                                                         , defaultParseMode
                                                         , extensions
                                                         , ignoreLanguagePragmas
                                                         , parseFilename
                                                         )
import           Longboye.Files                          ( Contents( WithSubject
                                                                   , WithoutSubject
                                                                   )
                                                         )

type Pragma = ModulePragma SrcSpanInfo

parse :: [Extension] -> FilePath -> Text -> Maybe (Contents [Pragma])
parse foundExtensions path = eitherToMaybe . parseE foundExtensions path

parseE :: [Extension] -> FilePath -> Text -> Either Text (Contents [Pragma])
parseE foundExtensions path source = case parseFileContentsWithMode parseMode sourceText of
  ParseOk parsedMod ->
    if null pragmas
      then Right . WithoutSubject $ source
      else Right . WithSubject    $ (prefix, pragmas, suffix)
    where
      pragmas = getPragmas parsedMod
      prefix  = extractPrefix parsedMod source
      suffix  = extractSuffix parsedMod source
  ParseFailed srcLoc' err ->
    Left . Text.pack $ "ERROR at " ++ show srcLoc' ++ ": " ++ err
  where
    parseMode = defaultParseMode
      { baseLanguage          = Haskell2010
      , ignoreLanguagePragmas = False
      , extensions            = configuredExtensions
      , parseFilename         = path
      }
    configuredExtensions = extensions defaultParseMode ++ foundExtensions
    sourceText           = Text.unpack source

-- TODO: simplify it we stick with `type Pragma = ModulePragma SrcSpanInfo`
getPragmas :: Module SrcSpanInfo -> [Pragma]
getPragmas = extractPragmas

-- TODO: DRY up vs other parses
extractPrefix :: Module SrcSpanInfo -> Text -> Text
extractPrefix XmlPage {}                        _ = notSupported "XmlPage"
extractPrefix XmlHybrid {}                      _ = notSupported "XmlHybrid"
extractPrefix (Module _ _ pragmaDecls _ _) source =
  Text.unlines . take (n - 1) . Text.lines $ source
  where
    n = srcSpanStartLine . srcInfoSpan . ann . Prelude.head $ pragmaDecls

extractSuffix :: Module SrcSpanInfo -> Text -> Text
extractSuffix XmlPage {}   _                      = notSupported "XmlPage"
extractSuffix XmlHybrid {} _                      = notSupported "XmlHybrid"
extractSuffix (Module _ _ pragmaDecls _ _) source =
  Text.unlines . drop n . Text.lines $ source
  where
    n = srcSpanEndLine . srcInfoSpan . ann . Prelude.last $ pragmaDecls

extractPragmas :: Module SrcSpanInfo -> [ModulePragma SrcSpanInfo]
extractPragmas (Module _l _ decls _ _) = decls
extractPragmas XmlHybrid {}            = notSupported "XmlHybrid"
extractPragmas XmlPage {}              = notSupported "XmlPage"
