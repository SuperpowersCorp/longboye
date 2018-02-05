{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Longboye.Main ( main ) where

import qualified Prelude
import           Longboye.Prelude

import           Control.Monad                    ( when )
import qualified Longboye.Imports      as Imports
import           Longboye.Interaction
import qualified Longboye.Pragmas      as Pragmas
import           System.Console.Docopt            ( Docopt
                                                  , argument
                                                  , command
                                                  , docoptFile
                                                  , exitWithUsage
                                                  , getAllArgs
                                                  , isPresent
                                                  , longOption
                                                  , parseArgsOrExit
                                                  )
import           System.Environment               ( getArgs )
import           System.Exit                      ( exitSuccess )

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

  when (opts `isPresent` command "imports") $ do
    case opts `getAllArgs` argument "path" of
      ["-"] -> Imports.interact
      ps
        | "-" `elem` ps -> panic cannotMixErr
        | otherwise     -> Imports.clean ps

  where
    version      = "0.0.0.1"
    cannotMixErr = "you cannot mix stdin (-) with files.  try one or the other."
