module Longboye.Modules ( clean, interact ) where

import           Prelude                                       hiding ( interact
                                                                      , readFile
                                                                      , writeFile
                                                                      )
import qualified Prelude

import           Control.Monad                                        ( foldM
                                                                      , void
                                                                      )
import           Data.List                                            ( isPrefixOf )
import           Data.Monoid                                          ( (<>) )
import           Data.Text                                            ( Text
                                                                      , unpack
                                                                      )
import qualified Data.Text                       as Text
import           Data.Text.IO                                         ( readFile
                                                                      , writeFile
                                                                      )
import           Language.Haskell.Exts.Extension                      ( Extension )
import qualified Longboye.Extensions             as Extensions
import           Longboye.ModuleStatement                             ( ModuleStatement )
import           Longboye.ModuleStatementParser                       ( Parsed( NoModuleStatement
                                                                              , WithModuleStatement
                                                                              )
                                                                      )
import qualified Longboye.ModuleStatementParser  as Parser
import           System.Directory                                     ( listDirectory
                                                                      , removeFile
                                                                      )
import           System.FilePath.Posix                                ( joinPath )
import           System.Posix.Files                                   ( getFileStatus
                                                                      , isDirectory
                                                                      , rename
                                                                      )

clean :: [FilePath] -> IO ()
clean []           = return ()
clean (path:paths) = cleanPath path >>= either abort continue
  where abort err  = error $ "An error occured: " ++ unpack err
        continue   = const $ clean paths

cleanPath :: FilePath -> IO (Either Text ())
cleanPath path = do
  putStrLn $ "checking status of path: " ++ path
  stat <- getFileStatus path
  if isDirectory stat
    then cleanDir path
    else cleanFile path

cleanDir :: FilePath -> IO (Either Text ())
cleanDir path = (filter (not . hidden) <$> listDirectory path) >>= foldM f (Right ())
  where f (Right ()) file = cleanPath (joinPath [path, file])
        f err _           = return err
        hidden = ("." `isPrefixOf`)

cleanFile :: FilePath -> IO (Either Text ())
cleanFile path = do
  putStrLn $ "Processing file: " ++ path
  contents <- readFile path
  foundExtensions <- Extensions.find path
  case Parser.parseE foundExtensions path contents of
    Left err                           -> return . Left $ err
    Right (NoModuleStatement _)        -> return . Right $ ()
    Right (WithModuleStatement parsed) -> Right <$> doCleaning path contents parsed

doCleaning :: FilePath -> Text -> (Text, ModuleStatement, Text) -> IO()
doCleaning path contents (prefix, moduleStatement, suffix) = do
  void $ writeFile backupPath contents
  let cleaned = cleanText prefix moduleStatement suffix
  writeFile tempPath cleaned
  void $ rename tempPath path
  void $ removeFile backupPath
  where backupPath = path ++ ".longboye.bak"
        tempPath   = path ++ ".longboye.tmp"


interact :: IO ()
interact = Extensions.find "." >>= Prelude.interact . interactS

interactS :: [Extension] -> String -> String
interactS extensions contents = Text.unpack $
  case Parser.parseE extensions "<interactive>" (Text.pack contents) of
    Left _ ->
      Text.pack contents
    Right (NoModuleStatement s) ->
      s
    Right (WithModuleStatement (prefix, moduleStatement, suffix)) ->
      cleanText prefix moduleStatement suffix

cleanText :: Text -> ModuleStatement -> Text -> Text
cleanText prefix moduleStatement suffix =
  formatPrefix prefix <> formatModuleStatement moduleStatement <> formatSuffix suffix
  where
    formatPrefix          = error "Modules.cleanText.formatPrefix not implemented."
    formatSuffix          = error "Modules.cleanText.formatSuffix not implemented."
    formatModuleStatement = error "Modules.cleanText.formatModuleStatement not implemented."
