{-# LANGUAGE OverloadedStrings #-}
module Longboye.Interaction ( runInteraction ) where

import           Control.Monad                   ( foldM
                                                 , void
                                                 )
import           Data.Char                       ( ord )
import           Data.List                       ( isPrefixOf
                                                 , sortBy
                                                 )
import           Data.Text                       ( Text )
import qualified Data.Text             as Text
import           Longboye.Import                 ( Import )
import           Longboye.Parser                 ( Parsed( NoImports
                                                         , WithImports
                                                         ) )
import qualified Longboye.Parser       as Parser
import           System.Directory                ( listDirectory
                                                 , removeFile
                                                 )
import           System.FilePath.Posix           ( joinPath )
import           System.Posix.Files              ( getFileStatus
                                                 , isDirectory
                                                 , rename
                                                 )

runInteraction :: (String -> String) -> [FilePath] -> IO ()
runInteraction cleanF ["-"] = interact cleanF
runInteraction cleanF paths =
  if "-" `elem` paths
    then error cannotMixErr
    else clean paths
  where cannotMixErr = "you cannot mix stdin (-) with files.  try one or the other."

        clean :: [FilePath] -> IO ()
        clean []           = return ()
        clean (path:paths) = cleanPath cleanF path >>= either abort continue
          where abort err  = error $ "An error occured: " ++ Text.unpack err
                continue   = const $ clean paths


cleanPath :: (String -> String) -> FilePath -> IO (Either Text ())
cleanPath cleanF path = do
  stat <- getFileStatus path
  if isDirectory stat
    then cleanDir path
    else (cleanFile cleanF) path

cleanDir :: FilePath -> IO (Either Text ())
cleanDir path = (filter (not . hidden) <$> listDirectory path) >>= foldM f (Right ())
  where f (Right ()) file = cleanPath (joinPath [path, file])
        f err _           = return err
        hidden = ("." `isPrefixOf`)

cleanFile :: (String -> String) -> FilePath -> IO (Either Text ())
cleanFile cleanF path = do
  putStrLn $ cuteMsg ++ "... " ++ path ++ " 🐶" -- <- mind the invisible unicode doggo
  contents <- readFile path
  case Parser.parseE path (Text.pack contents) of
    Left err                    -> return . Left $ err
    Right (NoImports _)         -> return . Right $ ()
    Right (WithImports parsed) -> Right <$> doCleaning cleanF path contents parsed
  where pseudoRandomN = sum . map ord $ path
        cuteMsg = cuteMessages !! randIndex
        randIndex = pseudoRandomN `mod` (length cuteMessages)
        cuteMessages = [ "Licking"
                       , "Chewing"
                       , "Biting"
                       , "Gnawing on"
                       , "Borking"
                       , "De-borking"
                       , "Re-borking"
                       ]

-- TODO: parameterize over parsed haskell-src-exts data structures, or better
-- yet, our clean internal ones...
doCleaning :: (String -> String) -> FilePath -> Text -> (Text, [Import], Text) -> IO ()
doCleaning cleanF path contents (prefix, imports, suffix) = do
  void $ writeFile backupPath (Text.unpack contents)
  let cleaned = cleanF prefix imports suffix
  writeFile tempPath cleaned
  void $ rename tempPath path
  void $ removeFile backupPath
  where backupPath = path ++ ".longboye.bak"
        tempPath   = path ++ ".longboye.tmp"

-- doCleaning :: FilePath -> Text -> (Text, [Import], Text) -> IO ()
-- doCleaning path contents (prefix, imports, suffix) = do
--   void $ writeFile backupPath contents
--   let cleaned = cleanText prefix imports suffix
--   writeFile tempPath cleaned
--   void $ rename tempPath path
--   void $ removeFile backupPath
--   where backupPath = path ++ ".longboye.bak"
--         tempPath   = path ++ ".longboye.tmp"

-- clean :: [FilePath] -> IO ()
-- clean []           = return ()
-- clean (path:paths) = cleanPath path >>= either abort continue
--   where abort err  = error $ "An error occured: " ++ unpack err
--         continue   = const $ clean paths

-- cleanPath :: FilePath -> IO (Either Text ())
-- cleanPath path = do
--   stat <- getFileStatus path
--   if isDirectory stat
--     then cleanDir path
--     else cleanFile path

-- cleanDir :: FilePath -> IO (Either Text ())
-- cleanDir path = (filter (not . hidden) <$> listDirectory path) >>= foldM f (Right ())
--   where f (Right ()) file = cleanPath (joinPath [path, file])
--         f err _           = return err
--         hidden = ("." `isPrefixOf`)

-- cleanFile :: FilePath -> IO (Either Text ())
-- cleanFile path = do
--   putStrLn $ cuteMsg ++ "... " ++ path ++ " 🐶" -- <- mind the invisible unicode doggo
--   contents <- readFile path
--   case Parser.parseE path contents of
--     Left err                    -> return . Left $ err
--     Right (NoImports _)         -> return . Right $ ()
--     Right (WithImports parsed) -> Right <$> doCleaning path contents parsed
--   where pseudoRandomN = sum . map ord $ path
--         cuteMsg = cuteMessages !! randIndex
--         randIndex = pseudoRandomN `mod` (length cuteMessages)
--         cuteMessages = [ "Licking"
--                        , "Chewing"
--                        , "Biting"
--                        , "Gnawing on"
--                        , "Borking"
--                        , "De-borking"
--                        , "Re-borking"
--                        ]
