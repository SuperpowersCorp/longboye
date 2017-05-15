{-# LANGUAGE QuasiQuotes     #-}
module Longboye.Main ( main ) where

import           Control.Monad         ( when )
import qualified Longboye.Imports      as Imports
import           System.Environment    ( getArgs )
import           System.Exit           ( exitSuccess )
import           System.Console.Docopt ( Docopt
                                       , argument
                                       , command
                                       , docoptFile
                                       , exitWithUsage
                                       , getAllArgs
                                       , isPresent
                                       , longOption
                                       , parseArgsOrExit
                                       )

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

main :: IO ()
main = main_ =<< getArgs

main_ :: [String] -> IO ()
main_ argv = do
  opts <- parseArgsOrExit patterns argv

  when (opts `isPresent` longOption "version") $ do
    putStrLn $ "Longboye v" ++ version
    exitSuccess

  when (opts `isPresent` longOption "help") $ exitWithUsage patterns

  when (opts `isPresent` command "imports") $ do
    let paths = opts `getAllArgs` argument "path"
    Imports.clean paths

  where version      = "0.0.0.1"
