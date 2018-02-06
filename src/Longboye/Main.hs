{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Longboye.Main ( main ) where

import qualified Prelude
import           Longboye.Prelude

import           Longboye.Files                          ( genericClean
                                                         , genericInteract
                                                         )
import qualified Longboye.Imports       as Imports
import qualified Longboye.ImportsParser as ImportsParser
import qualified Longboye.Pragmas       as Pragmas
import qualified Longboye.PragmasParser as PragmasParser
import           System.Console.Docopt                   ( Docopt
                                                         , argument
                                                         , command
                                                         , docoptFile
                                                         , exitWithUsage
                                                         , getAllArgs
                                                         , isPresent
                                                         , longOption
                                                         , parseArgsOrExit
                                                         )
import           System.Environment                      ( getArgs )
import           System.Exit                             ( exitSuccess )

-- import qualified Longboye.ModuleStatementParser as ModuleStatementParser
-- import qualified Longboye.ModuleStatements      as ModuleStatements

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

  when (opts `isPresent` command "imports") $
    case opts `getAllArgs` argument "path" of
      ["-"] -> genericInteract ImportsParser.parseE Imports.cleanText
      ps
        | "-" `elem` ps -> panic cannotMixErr
        | otherwise     -> genericClean ImportsParser.parseE Imports.cleanText ps

  -- when (opts `isPresent` command "modules") $
  --   case opts `getAllArgs` argument "path" of
  --     ["-"] -> genericInteract ModuleStatementParser.parseE ModuleStatements.cleanText
  --     ps
  --       | "-" `elem` ps -> panic cannotMixErr
  --       | otherwise     -> genericClean ModuleStatementParser.parseE ModuleStatements.cleanText ps

  when (opts `isPresent` command "pragmas") $
    case opts `getAllArgs` argument "path" of
      ["-"] -> genericInteract PragmasParser.parseE Pragmas.cleanText
      ps
        | "-" `elem` ps -> panic cannotMixErr
        | otherwise     -> genericClean PragmasParser.parseE Pragmas.cleanText ps

  where
    version      = "0.0.0.1"
    cannotMixErr = "you cannot mix stdin (-) with files.  try one or the other."
