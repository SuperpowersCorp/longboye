module Longboye.Imports ( clean ) where

import Prelude          hiding ( readFile
                               , writeFile
                               )

import Control.Monad           ( void )
import Data.Text               ( Text
                               , pack
                               , unpack
                               )
import Data.Text.IO            ( readFile
                               , writeFile
                               )
import System.Directory        ( removeFile )
import Longboye.Imports.Verify ( VerifiedTempPath
                               , tempContent
                               , newContent
                               )

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
  verifiedTempPath <- tempContent contents tempPath
  void $ swap verifiedTempPath path
  void $ newContent cleaned path
  void $ removeFile backupPath
  return . Right $ ()
  where backupPath      = path ++ ".lbak"
        tempPath        = path ++ ".ltemp"
        abortVerify err = error $ "Temporary file verification failed: " ++ unpack err

backup :: Text -> FilePath -> IO ()
backup = flip writeFile

swap :: VerifiedTempPath -> FilePath -> IO ()
swap = error "swap is not implemented."

cleanText :: Text -> Text
cleanText src = src -- TODO:
