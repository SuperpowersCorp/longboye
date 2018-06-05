{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LB20 where

import           Prelude                                           ( String )
import           Longboye.Prelude                           hiding ( mod )
import qualified Streaming.Prelude               as S

import           Data.Text                                         ( pack
                                                                   , unpack
                                                                   )
import qualified LB20.Imports                    as Imports
import qualified LB20.Pragmas                    as Pragmas
import           Language.Haskell.Exts                             ( Comment
                                                                   , Module
                                                                   , SrcSpanInfo
                                                                   , exactPrint
                                                                   , parseFileContentsWithComments
                                                                   , parseFileContentsWithMode
                                                                   )
import           Language.Haskell.Exts.Extension                   ( Extension
                                                                   , Language( Haskell2010 )
                                                                   )
import           Language.Haskell.Exts.Parser                      ( ParseResult( ParseFailed
                                                                                , ParseOk
                                                                                )
                                                                   , baseLanguage
                                                                   , defaultParseMode
                                                                   , extensions
                                                                   , ignoreLanguagePragmas
                                                                   , parseFilename
                                                                   )
import qualified Longboye.Extensions             as Exts
import           Options.Applicative
import           Streaming.Files
import           System.Posix.Files                                ( isDirectory )
import           Text.PrettyPrint.ANSI.Leijen                      ( Doc
                                                                   , linebreak
                                                                   , text
                                                                   )

data Cmd
  = CmdAll FilePath
  | CmdImports FilePath
  | CmdPragmas FilePath

type Formatter = Module SrcSpanInfo -> Module SrcSpanInfo

main :: IO ()
main = execParser opts >>= \case
  CmdAll     path -> formatAll path [ Imports.format
                                    , Pragmas.format
                                    ]
  CmdImports path -> formatAll path [ Imports.format ]
  CmdPragmas path -> formatAll path [ Pragmas.format ]
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
  foundExtensions <- Exts.find path
  S.mapM_ (formatOne foundExtensions formatters)
    . S.map fst
    . S.chain (putLn . ("Gnawing on... " <>) . (<> doggo) . pack . fst)
    . S.filter (not . isDirectory . snd)
    . tree
    $ path
  where
    doggo = " 🐶" -- <- mind the invisible unicode doggo

formatOne :: [Extension] -> [Formatter] -> FilePath -> IO ()
formatOne exts formatters path = do
  source <- readFile path
  case parseSource exts path source of
    ParseFailed srcLoc' msg ->
      panic $ "FAILURE at " <> show srcLoc' <> ": " <> pack msg
    ParseOk (mod, comments) ->
      safeWriteFile path . renderParsed comments . applyFormatters formatters $ mod
  where
    renderParsed :: [Comment] -> Module SrcSpanInfo -> String
    renderParsed = flip exactPrint

applyFormatters :: [Formatter] -> Module SrcSpanInfo -> Module SrcSpanInfo
applyFormatters formatters mod = foldl (&) mod formatters

parseSource :: [Extension] -> FilePath -> Text
            -> ParseResult (Module SrcSpanInfo, [Comment])
parseSource exts path src = do
  mod <- parseFileContentsWithMode parseMode . unpack $ src
  (_, comments) <- parseFileContentsWithComments parseMode . unpack $ src
  return $ (mod, comments)
  where
    parseMode = defaultParseMode
      { baseLanguage          = Haskell2010           -- TODO read from cabal
      , ignoreLanguagePragmas = False
      , extensions            = exts
      , parseFilename         = path
      }

-- TODO: the whole swapping in place dance
safeWriteFile :: FilePath -> String -> IO ()
safeWriteFile path = writeFile path . pack
