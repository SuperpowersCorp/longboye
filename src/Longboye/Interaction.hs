{-# LANGUAGE OverloadedStrings #-}
module Longboye.Interaction ( runInteraction ) where

import           Control.Monad                         ( foldM
                                                       , void
                                                       )
import           Data.Char                             ( ord )
import           Data.List                             ( isPrefixOf )
import           Data.Text                             ( Text )
import qualified Data.Text              as Text
import qualified Data.Text.IO           as TextIO
import           Longboye.Extensions    as Extensions
import           Longboye.Import                       ( Import )
import           Longboye.ImportsParser                ( Parsed( NoImports
                                                               , WithImports
                                                               )
                                                       )
import qualified Longboye.ImportsParser as Parser
import           Longboye.Transformer                  ( Transformer )
import qualified Longboye.Transformer   as Transformer
import           System.Directory                      ( listDirectory
                                                       , removeFile
                                                       )
import           System.FilePath.Posix                 ( joinPath )
import           System.Posix.Files                    ( getFileStatus
                                                       , isDirectory
                                                       , rename
                                                       )

runInteraction :: Transformer -> [FilePath] -> IO ()
runInteraction _xform ["-"] = TextIO.interact f
  where f = error "runInteraction not implemented."
  -- let context = Transformer.context "<stdin>" contents prefix imports suffix
  --     xformed = xform context
  -- where f contents = error "finish implementing me."
  --         where (prefix, imports, suffix) =
  --                 case Parser.parseE path contents of
  --                   Left err                   -> error err
  --                   Right (NoImports _)        -> return . Right $ ()
  --                   Right (WithImports parsed) -> parsed
  --               context = Transformer.context "<stdin>" contents prefix imports suffix

runInteraction xform paths =
  if "-" `elem` paths
    then error cannotMixErr
    else clean paths
  where cannotMixErr = "you cannot mix stdin (-) with files.  try one or the other."

        clean :: [FilePath] -> IO ()
        clean []     = return ()
        clean (p:ps) = cleanPath xform p >>= either abort continue
          where abort err = error $ "An error occured: " ++ Text.unpack err
                continue  = const $ clean ps

cleanPath :: Transformer -> FilePath -> IO (Either Text ())
cleanPath xform path = do
  stat <- getFileStatus path
  if isDirectory stat
    then cleanDir xform path
    else cleanFile xform path

cleanDir :: Transformer -> FilePath -> IO (Either Text ())
cleanDir xform path = (filter (not . hidden) <$> listDirectory path) >>= foldM f (Right ())
  where f (Right ()) file = cleanPath xform (joinPath [path, file])
        f err _           = return err
        hidden = ("." `isPrefixOf`)

cleanFile :: Transformer -> FilePath -> IO (Either Text ())
cleanFile xform path = do
  putStrLn $ cuteMsg ++ "... " ++ path ++ " 🐶" -- <- mind the invisible unicode doggo
  contents <- TextIO.readFile path
  -- TODO: memoize
  extensions <- Extensions.find path
  case Parser.parseE extensions path contents of
    Left err                   -> return . Left $ err
    Right (NoImports _)        -> return . Right $ ()
    Right (WithImports parsed) -> applyCleanF xform path contents parsed
  where pseudoRandomN = sum . map ord $ path
        cuteMsg       = cuteMessages !! randIndex
        randIndex     = pseudoRandomN `mod` (length cuteMessages)
        cuteMessages  = [ "Licking"
                        , "Chewing"
                        , "Biting"
                        , "Gnawing on"
                        , "Borking"
                        , "De-borking"
                        , "Re-borking"
                        ]

applyCleanF :: Transformer -> FilePath -> Text -> (Text, [Import], Text) -> IO (Either Text ())
applyCleanF xform path contents (prefix, imports, suffix) = do
  void $ TextIO.writeFile backupPath contents
  let context = Transformer.context path contents prefix imports suffix
      xformed = xform context
  TextIO.writeFile tempPath xformed
  void $ rename tempPath path
  void $ removeFile backupPath
  return $ Right ()
  where backupPath = path ++ ".longboye.bak"
        tempPath   = path ++ ".longboye.tmp"
