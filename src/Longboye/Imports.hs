module Longboye.Imports ( clean ) where

import           Prelude           hiding ( readFile
                                          , writeFile
                                          )

import           Control.Monad            ( void )
import           Data.Monoid              ( (<>) )
import           Data.Text                ( Text
                                          , unpack
                                          )
import qualified Data.Text                as Text
import           Data.Text.IO             ( readFile
                                          , writeFile
                                          )
import           Longboye.Import          ( Import )
import qualified Longboye.Import          as Import
import           Longboye.Imports.Cracker ( Cracked( NoImports
                                                   , WithImports
                                                   )
                                          )
import qualified Longboye.Imports.Cracker as Cracker
import           Longboye.Imports.Verify  ( VerifiedTempPath )
import qualified Longboye.Imports.Verify  as Verify
import           System.Directory         ( removeFile )
import           System.Posix.Files       ( rename )

clean :: [FilePath] -> IO ()
clean []           = return ()
clean (path:paths) = cleanFile path >>= either abort continue
  where abort err  = error $ "An error occured: " ++ unpack err
        continue   = const $ clean paths

cleanFile :: FilePath -> IO (Either Text ())
cleanFile path = do
  -- putStrLn $ "Processing file: " ++ path
  contents <- readFile path
  case Cracker.crackE path contents of
    Left err                    -> do
      -- putStrLn $ "ERROR: " ++ unpack err
      return . Left $ err
    Right (NoImports _)         -> do
      -- putStrLn "No imports, so no changes..."
      return . Right $ ()
    Right (WithImports cracked) -> do
      -- putStrLn "Found imports, so doing cleaning..."
      doCleaning path contents cracked >>= return . Right

doCleaning :: FilePath -> Text -> (Text, [Import], Text) -> IO ()
doCleaning path contents (prefix, imports, suffix) = do
  -- putStrLn $ "Writing backup @ " ++ backupPath
  void $ writeFile backupPath contents

  let cleaned = cleanText prefix imports suffix

  -- putStrLn $ "Writing cleaned @ " ++ tempPath
  writeFile tempPath cleaned

  -- putStrLn $ "Verifying contents @ " ++ tempPath
  verifiedTempPath <- Verify.tempContent contents tempPath

  -- putStrLn $ "Swapping temp file back @ " ++ path
  void $ swap verifiedTempPath path

  -- putStrLn $ "Verifying new content @ " ++ path
  void $ Verify.newContent cleaned path

  -- putStrLn $ "Removing backup file @ " ++ backupPath
  void $ removeFile backupPath

  where backupPath      = path ++ ".lbak"
        tempPath        = path ++ ".ltemp"

swap :: VerifiedTempPath -> FilePath -> IO ()
swap vtp path = rename src dst
  where src = Verify.accessPath $ vtp
        dst = path

cleanText :: Text -> [Import] -> Text -> Text
cleanText prefix imports suffix =
  (formatPrefix prefix) <> (formatImports imports) <> (formatSuffix suffix)
  where formatPrefix  = (<> "\n\n") . Text.stripEnd
        formatSuffix  = ("\n" <>)   . Text.stripStart
        formatImports = Text.unlines . map (Import.format maxModLen maxAsLen)
        maxModLen     = maximum . map (Text.length . Import.importedModule) $ imports
        maxAsLen      = maximum . map Import.asLength                       $ imports
