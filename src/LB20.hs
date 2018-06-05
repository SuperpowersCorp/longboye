{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LB20 where

import           Prelude                                        ( String )
import           Longboye.Prelude                        hiding ( mod )
import qualified Streaming.Prelude                as S

import           Data.Text                                      ( pack
                                                                , unpack
                                                                )
import           Language.Haskell.Exts                          ( SrcSpanInfo
                                                                , Module
                                                                , parseFileContentsWithMode
                                                                , prettyPrint
                                                                )
import           Language.Haskell.Exts.Extension                ( Extension
                                                                , Language(Haskell2010)
                                                                )
import qualified Longboye.Extensions              as Extensions
import           Options.Applicative
import           Streaming.Files
import           System.Posix.Files                             ( isDirectory )
import           Text.PrettyPrint.ANSI.Leijen                   ( Doc
                                                                , linebreak
                                                                , text
                                                                )
import           Language.Haskell.Exts.Parser                   ( ParseResult( ParseFailed
                                                                             , ParseOk
                                                                             )
                                                                , baseLanguage
                                                                , defaultParseMode
                                                                , extensions
                                                                , ignoreLanguagePragmas
                                                                , parseFilename
                                                                )

data Cmd
  = CmdAll FilePath
  | CmdImports FilePath
  | CmdPragmas FilePath

data Formatter = Formatter

data Debug = DebugOff | DebugOn

debugMode :: Debug
-- debugMode = DebugOff
debugMode = DebugOn

main :: IO ()
main = execParser opts >>= \case
  CmdAll     path -> formatAll path allFormatters
  CmdImports path -> formatAll path [importsFormatter]
  CmdPragmas path -> formatAll path [pragmasFormatter]
  where
    opts :: ParserInfo Cmd
    opts = info (helper <*> parseCmd) (fullDesc <> headerDoc (Just woof))

    parseCmd :: Parser Cmd
    parseCmd = subparser . foldMap command' $
      [ ("all"    , "Format all the things", CmdAll     <$> targetPath)
      , ("imports", "Format only imports"  , CmdImports <$> targetPath)
      , ("pragmas", "Format only pragmas"  , CmdPragmas <$> targetPath)
      ]

    targetPath = argument str
      ( metavar "TARGET"
     <> help "The path to a file or directory to process."
      )

    command' :: (String, String, Parser Cmd) -> Mod CommandFields Cmd
    command' (name, desc, parser) = command name (info' parser desc)

    info' :: Parser a -> String -> ParserInfo a
    info' p desc = info (helper <*> p) (fullDesc <> progDesc desc)

    allFormatters = [importsFormatter, pragmasFormatter]
    importsFormatter = Formatter
    pragmasFormatter = Formatter

    woof :: Doc
    woof = mconcat
      [ text "Longboye"
      , linebreak
      , text ""
      , linebreak
      , text "             /)-_-(\\"
      , linebreak
      , text "              (o o)        \"Everything I know, I learned from dogs.\""
      , linebreak
      , text "      .-----__/\\o/                             - Nora Roberts"
      , linebreak
      , text "     /  __      /"
      , linebreak
      , text " \\__/\\ /  \\_\\ |/"
      , linebreak
      , text "      \\     ||"
      , linebreak
      , text "      //     ||"
      , linebreak
      , text "      |\\     |\\"
      , linebreak
      ]

formatAll :: FilePath -> [Formatter] -> IO ()
formatAll path formatters = do
  foundExtensions <- Extensions.find path
  S.mapM_ (formatOne foundExtensions formatters)
    . S.map fst
    . S.chain (debugLn . ("Formatting: " <>) . pack . fst)
    . S.filter (not . isDirectory . snd)
    . S.chain (debugLn . ("Processing: " <>) . pack . fst)
    . tree
    $ path
  where
    debugLn s = case debugMode of
      DebugOn  -> putLn $ "DEBUG: " <> s
      DebugOff -> return ()

formatOne :: [Extension] -> [Formatter] -> FilePath -> IO ()
formatOne exts formatters path = do
  source <- readFile path
  case parseSource exts path source of
    ParseFailed srcLoc' msg -> panic $ "FAILURE at " <> show srcLoc' <> ": " <> pack msg
    ParseOk mod -> writeFile path . renderParsed . applyFormatters formatters $ mod

applyFormatters :: [Formatter] -> Module SrcSpanInfo -> Module SrcSpanInfo
applyFormatters formatters mod = foldl f mod formatters
  where
    f :: Module SrcSpanInfo -> Formatter -> Module SrcSpanInfo
    f mod' _formatter = mod'
    -- TODO: actually apply formatters

parseSource :: [Extension] -> FilePath -> Text -> ParseResult (Module SrcSpanInfo)
parseSource exts path = parseFileContentsWithMode parseMode . unpack
  where
    parseMode = defaultParseMode
      { baseLanguage          = Haskell2010
      , ignoreLanguagePragmas = False
      , extensions            = exts
      , parseFilename         = path
      }

renderParsed :: Module SrcSpanInfo -> Text
renderParsed = pack . prettyPrint
