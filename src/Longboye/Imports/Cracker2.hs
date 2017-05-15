module Longboye.Imports.Cracker2 ( crack, crackE ) where

import           Data.Text                    ( Text )
import qualified Data.Text                    as Text
import           Language.Haskell.Exts        ( Module( Module
                                                      , XmlHybrid
                                                      , XmlPage
                                                      )
                                              , SrcSpanInfo
                                              )
import           Language.Haskell.Exts.Parser ( ParseResult( ParseOk
                                                           , ParseFailed
                                                           )
                                              , defaultParseMode
                                              , parseFilename
                                              )
import qualified Language.Haskell.Exts.Parser as Parser
import           Longboye.Import              ( Import )
import           Overture

data Cracked
  = NoImports Text
  | WithImports (Text, [Import], Text)
  deriving (Eq, Ord, Read, Show)

crack :: FilePath -> Text -> Maybe Cracked
crack path = eitherToMaybe . crackE path

crackE :: FilePath -> Text -> Either Text Cracked
crackE path source = case Parser.parseModuleWithMode parseMode sourceText of
  ParseOk parsedMod ->
    if null imports
      then Right . NoImports   $ source
      else Right . WithImports $ (prefix, imports, suffix)
    where imports = getImports parsedMod
          prefix  = extractPrefix source
          suffix  = extractSuffix source
  ParseFailed srcLoc err ->
    Left . Text.pack $ "ERROR at " ++ show srcLoc ++ ": " ++ err
  where parseMode  = defaultParseMode { parseFilename = path }
        sourceText = Text.unpack source

extractPrefix :: Text -> Text
extractPrefix = error "extractPrefix not implemented."

extractSuffix :: Text -> Text
extractSuffix = error "extractSuffix not implemented."

getImports :: Module SrcSpanInfo -> [Import]
getImports (XmlHybrid _ _ _ _ _ _ _ _ _)                  = notSupported "XmlHybrid"
getImports (XmlPage _ _ _ _ _ _ _)                        = notSupported "XmlPage"
getImports (Module _l _mHead _pragmas importDecls _decls) = convert importDecls
  where convert _decls = [] -- TODO: implement me

notSupported :: String -> a
notSupported = error . (++ " modules not supported.")
