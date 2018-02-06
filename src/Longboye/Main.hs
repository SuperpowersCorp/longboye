{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Longboye.Main ( main ) where

import qualified Prelude
import           Longboye.Prelude

import           Longboye.Files                                          ( genericClean
                                                                         , genericInteract
                                                                         )
import qualified Longboye.Imports               as Imports
import qualified Longboye.ImportsParser         as ImportsParser
import qualified Longboye.ModuleStatementParser as ModuleStatementParser
import qualified Longboye.ModuleStatements      as ModuleStatements
import qualified Longboye.Pragmas               as Pragmas
import qualified Longboye.PragmasParser         as PragmasParser
import           System.Console.Docopt                                   ( Docopt
                                                                         , argument
                                                                         , command
                                                                         , docoptFile
                                                                         , exitWithUsage
                                                                         , getAllArgs
                                                                         , isPresent
                                                                         , longOption
                                                                         , parseArgsOrExit
                                                                         )
import           System.Environment                                      ( getArgs )
import           System.Exit                                             ( exitSuccess )

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

main :: IO ()
main = main_ =<< getArgs

main_ :: [Prelude.String] -> IO ()
main_ argv = do
  opts <- parseArgsOrExit patterns argv

  when (opts `isPresent` longOption "version") $ do
    --           v-- mind the invisible unicode doggo
    putLn $ "🐕  Longboye v" <> version
    exitSuccess

  when (opts `isPresent` longOption "help") $
    exitWithUsage patterns

  whenRequested opts "imports" ImportsParser.parseE         Imports.cleanText
  whenRequested opts "modules" ModuleStatementParser.parseE ModuleStatements.cleanText
  whenRequested opts "pragmas" PragmasParser.parseE         Pragmas.cleanText

  where
    version      = "0.0.0.1"
    cannotMixErr = "you cannot mix stdin (-) with files.  try one or the other."

    whenRequested opts cmd parse clean =
      when (opts `isPresent` command cmd) $
        case opts `getAllArgs` argument "path" of
          ["-"] -> genericInteract parse clean
          ps
            | "-" `elem` ps -> panic cannotMixErr
            | otherwise     -> genericClean parse clean ps
