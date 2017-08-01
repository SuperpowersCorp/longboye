module Longboye.ModuleStatementParser
       ( Parsed(..)
       , parse
       , parseE
       ) where

import           Overture

import           Data.Text                                          ( Text )
import qualified Data.Text                       as Text
import           Language.Haskell.Exts                              ( ModuleHead( ModuleHead )
                                                                    , Module( Module
                                                                            , XmlHybrid
                                                                            , XmlPage
                                                                            )
                                                                    , SrcSpanInfo
                                                                    , ann
                                                                    , parseFileContentsWithMode
                                                                    , srcInfoSpan
                                                                    , srcSpanEndLine
                                                                    , srcSpanStartLine
                                                                    )
import           Language.Haskell.Exts.Extension                    ( Extension
                                                                    , Language( Haskell2010 )
                                                                    )
import           Language.Haskell.Exts.Parser                       ( ParseResult( ParseOk
                                                                                 , ParseFailed
                                                                                 )
                                                                    , baseLanguage
                                                                    , defaultParseMode
                                                                    , extensions
                                                                    , ignoreLanguagePragmas
                                                                    , parseFilename
                                                                    )
import qualified Longboye.Errors                 as Errors
import           Longboye.ModuleStatement                           ( ModuleStatement )
import qualified Longboye.ModuleStatement        as ModuleStatement

data Parsed
  = NoModuleStatement Text
  | WithModuleStatement (Text, ModuleStatement, Text)
  deriving (Eq, Ord, Show)

parse :: [Extension] -> FilePath -> Text -> Maybe Parsed
parse foundExtensions path = eitherToMaybe . parseE foundExtensions path

parseE :: [Extension] -> FilePath -> Text -> Either Text Parsed
parseE foundExtensions path source = case parseFileContentsWithMode parseMode sourceText of
  ParseOk parsedMod ->
    case moduleStatementMay of
      Just moduleStatement ->
        Right . WithModuleStatement $ (prefix, moduleStatement, suffix)
      Nothing ->
        Right . NoModuleStatement $ source
    where
      moduleStatementMay = getModuleStatement parsedMod
      prefix             = extractPrefix parsedMod source
      suffix             = extractSuffix parsedMod source

  ParseFailed srcLoc err ->
    Left $ Errors.renderError srcLoc err
  where
    parseMode = defaultParseMode { baseLanguage          = Haskell2010
                                 , ignoreLanguagePragmas = False
                                 , extensions            = configuredExtensions
                                 , parseFilename         = path
                                 }
    configuredExtensions = extensions defaultParseMode ++ foundExtensions
    sourceText = Text.unpack source

getModuleStatement :: Module SrcSpanInfo -> Maybe ModuleStatement
getModuleStatement = Just . ModuleStatement.fromDecl

extractPrefix :: Module SrcSpanInfo -> Text -> Text
extractPrefix XmlPage {}                        _ = notSupported "XmlPage"
extractPrefix XmlHybrid {}                      _ = notSupported "XmlHybrid"
extractPrefix (Module _ modHeadMay _ _ _) source  =
  Text.unlines . take (n - 1) . Text.lines $ source
  where
    n = srcSpanStartLine . srcInfoSpan . ann $ modName'
    Just (ModuleHead _ modName' _ _) = modHeadMay

extractSuffix :: Module SrcSpanInfo -> Text -> Text
extractSuffix XmlPage {}   _                      = notSupported "XmlPage"
extractSuffix XmlHybrid {} _                      = notSupported "XmlHybrid"
extractSuffix (Module _ modHeadMay _ _ _) source =
  Text.unlines . drop n . Text.lines $ source
  where
    n = srcSpanEndLine . srcInfoSpan . ann $ modName'
    Just (ModuleHead _ modName' _ _) = modHeadMay
