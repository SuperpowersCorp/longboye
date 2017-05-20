module Longboye.Parser
       ( Parsed(..)
       , parse
       , parseE
       ) where

import           Overture

import           Data.Text                              ( Text )
import qualified Data.Text                    as Text
import           Language.Haskell.Exts                  ( Module( Module
                                                                , XmlHybrid
                                                                , XmlPage
                                                                )
                                                       
                                                        , SrcSpanInfo
                                                        , srcSpanEndLine
                                                        , srcSpanStartLine
                                                        , srcInfoSpan
                                                        , importAnn
                                                        )
import           Language.Haskell.Exts.Parser           ( ParseResult( ParseOk
                                                                     , ParseFailed
                                                                     )
                                                       
                                                        , defaultParseMode
                                                        , parseFilename
                                                        )
import qualified Language.Haskell.Exts.Parser as Parser
import           Language.Haskell.Exts.Syntax           ( ImportDecl )
import           Longboye.Import                        ( Import )
import qualified Longboye.Import              as Import

data Parsed
  = NoImports Text
  | WithImports (Text, [Import], Text)
  deriving (Eq, Ord, Read, Show)

parse :: FilePath -> Text -> Maybe Parsed
parse path = eitherToMaybe . parseE path

parseE :: FilePath -> Text -> Either Text Parsed
parseE path source = case Parser.parseModuleWithMode parseMode sourceText of
  ParseOk parsedMod ->
    if null imports
      then Right . NoImports   $ source
      else Right . WithImports $ (prefix, imports, suffix)
    where imports = getImports parsedMod
          prefix  = extractPrefix parsedMod source
          suffix  = extractSuffix parsedMod source
  ParseFailed srcLoc err ->
    Left . Text.pack $ "ERROR at " ++ show srcLoc ++ ": " ++ err
  where parseMode  = defaultParseMode { parseFilename = path }
        sourceText = Text.unpack source

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

getImports :: Module SrcSpanInfo -> [Import]
getImports = map Import.fromDecl <$> extractImports

notSupported :: String -> a
notSupported = error . (++ " modules not supported.")