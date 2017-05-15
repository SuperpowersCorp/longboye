module Longboye.Imports.Verify
       ( VerifiedTempPath
       , accessPath
       , newContent
       , tempContent
       ) where

import           Control.Monad              ( unless )
import           Data.Text                  ( Text
                                            , pack
                                            )
import qualified Data.Text                  as Text
-- import qualified Longboye.Imports.Cracker   as Cracker
import qualified Longboye.Imports.Cracker2  as Cracker

data VerifiedTempPath = VerifiedTempPath FilePath

accessPath :: VerifiedTempPath -> FilePath
accessPath (VerifiedTempPath path) = path

-- Needs to verify that `suspect` has the same content as `original` except for
-- the imports.
tempContent :: Text -> FilePath -> IO VerifiedTempPath
tempContent original path =
  verify <$> readFile path >>= either reportError return
  where
    reportError err =
      error $ "ERROR: Failed to verify tempContent at " ++ path ++ "\n" ++ show err ++ "\n"
    verify suspect = do
      (op, _, os) <- Cracker.crackE oFilename original
      (sp, _, ss) <- Cracker.crackE sFilename (pack suspect)
      return $ if op == sp && os == ss
        then VerifiedTempPath $ path
        else error "Temporary content could not be verified."

    oFilename = "original"
    sFilename = path

    -- lame, but for now:
    -- verify suspect =
    --   case Cracker.crack originalFilename original of
    --     Nothing -> error "Could not crack original."
    --     Just (op, _, os) -> case Cracker.crack suspectFilename suspect of
    --       Nothing -> error "Could not crack suspect."
    --       Just (sp, _, ss) ->
    --         if op == sp && os == ss
    --           then VerifiedTempPath path
    --           else error "Temporary content could not be verified."

newContent :: Text -> FilePath -> IO ()
newContent content path = do
  newContents <- Text.pack <$> readFile path
  unless (newContents == content) $ error errMsg
    where errMsg = "newContent could not be verified at " ++ path
