module Longboye.Imports ( clean ) where

import           Prelude          hiding ( putStrLn
                                         , readFile
                                         , writeFile
                                         )

import           Control.Monad           ( void )
import           Data.Text               ( Text
                                         , unpack
                                         )
import           Data.Text.IO            ( readFile
                                         , writeFile
                                         )
import           Longboye.Imports.Verify ( VerifiedTempPath )
import qualified Longboye.Imports.Verify as Verify
import           System.Directory        ( removeFile )
import           System.Posix.Files      ( rename )

clean :: [FilePath] -> IO ()
clean []           = return ()
clean (path:paths) = cleanFile path >>= either abort continue
  where abort err  = error $ "An error occured: " ++ unpack err
        continue   = const $ clean paths

cleanFile :: FilePath -> IO (Either Text ())
cleanFile path = do
  contents <- readFile path
  void $ backup contents backupPath
  let cleaned = cleanText contents
  writeFile tempPath cleaned
  verifiedTempPath <- Verify.tempContent contents tempPath
  void $ swap verifiedTempPath path
  void $ Verify.newContent cleaned path
  void $ removeFile backupPath
  return . Right $ ()
  where backupPath      = path ++ ".lbak"
        tempPath        = path ++ ".ltemp"

backup :: Text -> FilePath -> IO ()
backup = flip writeFile

swap :: VerifiedTempPath -> FilePath -> IO ()
swap vtp path = rename src dst
  where src = Verify.accessPath $ vtp
        dst = path

cleanText :: Text -> Text
cleanText src = src -- TODO:
