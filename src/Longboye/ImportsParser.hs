module Longboye.ImportsParser
       ( Parsed(..)
       , parse
       , parseE
       ) where

import           Overture

import           Data.Text                                 ( Text )
import qualified Data.Text                       as Text
import           Language.Haskell.Exts                     ( parseFileContentsWithMode )
import           Language.Haskell.Exts.Extension           ( Extension
                                                           , Language( Haskell2010 )
                                                           )
import           Language.Haskell.Exts.Parser              ( ParseResult( ParseOk
                                                                        , ParseFailed
                                                                        )
                                                           , baseLanguage
                                                           , defaultParseMode
                                                           , extensions
                                                           , ignoreLanguagePragmas
                                                           , parseFilename
                                                           )
import qualified Longboye.Errors                 as Errors
import           Longboye.Import                           ( Import )
import           Longboye.SrcSpans                         ( extractPrefix
                                                           , extractSuffix
                                                           , getImports
                                                           )

data Parsed
  = NoImports Text
  | WithImports (Text, [Import], Text)
  deriving (Eq, Ord, Read, Show)

parse :: [Extension] -> FilePath -> Text -> Maybe Parsed
parse foundExtensions path = eitherToMaybe . (parseE foundExtensions path)

parseE :: [Extension] -> FilePath -> Text -> Either Text Parsed
parseE foundExtensions path source = case parseFileContentsWithMode parseMode sourceText of
  ParseOk parsedMod ->
    if null imports
      then Right . NoImports   $ source
      else Right . WithImports $ (prefix, imports, suffix)
    where imports = getImports parsedMod
          prefix  = extractPrefix parsedMod source
          suffix  = extractSuffix parsedMod source
  ParseFailed srcLoc err ->
    Left $ Errors.renderError srcLoc err
  where parseMode = defaultParseMode { baseLanguage          = Haskell2010
                                     , ignoreLanguagePragmas = False
                                     , extensions            = configuredExtensions
                                     , parseFilename         = path
                                     }
        configuredExtensions = (extensions defaultParseMode) ++ foundExtensions
        sourceText           = Text.unpack source
