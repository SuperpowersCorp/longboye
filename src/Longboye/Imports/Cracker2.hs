module Longboye.Imports.Cracker2 ( crack ) where

import           Data.Text                    ( Text )
import           Longboye.Import              ( Import )
import qualified Longboye.Import              as Import

import           Language.Haskell.Exts
import           Language.Haskell.Exts.Pretty -- ( prettyPrint )
import           Data.Maybe                   -- ()
import           System.Environment           -- ()

type Cracked = (Text, [Import], Text)

crack :: String -> Text -> Maybe Cracked
crack filename s = undefined

-- parseModuleFromFile :: String -> Module srcSpanInfo
parseModuleFromFile inp = fromParseResult $ parseFileContents inp

main :: IO ()
main = main_ =<< getArgs

main_ :: [String] -> IO ()
main_ []     = getContents   >>= processSource
main_ [file] = readFile file >>= processSource
main_ _      = usage

processSource :: String -> IO ()
processSource src = do
  let mod = parseModuleFromFile src
  putStrLn $ prettyPrint mod

usage :: IO ()
usage = putStrLn "usage soon"

index n xs =
  case drop n xs of
    x:_ -> Just x
    []  -> Nothing
