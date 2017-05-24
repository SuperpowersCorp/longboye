{-# LANGUAGE QuasiQuotes     #-}
module Longboye.Main ( main ) where

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

main_ :: [String] -> IO ()
main_ argv = do
  opts <- parseArgsOrExit patterns argv

  when (opts `isPresent` longOption "version") $ do
    --           v-- mind the invisible unicode doggo
    putStrLn $ "🐕  Longboye v" ++ version
    exitSuccess

  when (opts `isPresent` longOption "help") $
    exitWithUsage patterns

  when (opts `isPresent` command "imports") $ do
    let paths = opts `getAllArgs` argument "path"
    runInteraction Imports.interactS paths

  when (opts `isPresent` command "pragmas") $ do
    let paths = opts `getAllArgs` argument "path"
    runInteraction Pragmas.interactS paths

  where version      = "0.0.0.1"
