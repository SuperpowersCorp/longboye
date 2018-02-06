{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.Files
  ( Contents(..)
  , genericClean
  , genericInteract
  , mkInteractor
  ) where

import qualified Prelude
import           Longboye.Prelude                              hiding ( interact )

import           Data.Text                                            ( pack )
import qualified Data.Text                       as Text
import           Data.Text.IO                                         ( readFile
                                                                      , writeFile
                                                                      )
import           Language.Haskell.Exts.Extension                      ( Extension )
import qualified Longboye.Extensions             as Extensions
import           System.Directory                                     ( listDirectory
                                                                      , removeFile
                                                                      )
import           System.FilePath.Posix                                ( joinPath )
import           System.Posix.Files                                   ( getFileStatus
                                                                      , isDirectory
                                                                      , rename
                                                                      )

type Cleaner a = Text -> a -> Text -> Text


type Parser a = [Extension] -> FilePath -> Text -> Either ParserError (Contents a)

type Interactor a = [Extension] -> Prelude.String -> Prelude.String


data Contents a
  = WithoutSubject Text
  | WithSubject (Text, a, Text)
  deriving (Eq, Ord, Read, Show)

type ParserError = Text

genericClean :: Parser a -> Cleaner a -> [FilePath] -> IO ()
genericClean _ _ []                   = return ()
genericClean parse clean (path:paths) = cleanPath parse clean path >>= either abort continue
  where
    abort err = panic $ "An error occured: " <> err
    continue  = const $ genericClean parse clean paths

cleanPath :: Parser a -> Cleaner a -> FilePath -> IO (Either Text ())
cleanPath parse clean path = do
  stat <- getFileStatus path
  if isDirectory stat
    then cleanDir  parse clean path
    else cleanFile parse clean path

cleanDir :: Parser a -> Cleaner a -> FilePath -> IO (Either Text ())
cleanDir parse clean path = (filter (not . hidden) <$> listDirectory path) >>= foldM f (Right ())
  where
    f (Right ()) file = cleanPath parse clean (joinPath [path, file])
    f err _           = return err
    hidden            = ("." `isPrefixOf`)

cleanFile :: Parser a -> Cleaner a -> FilePath -> IO (Either Text ())
cleanFile parse clean path = do
  putLn $ msg <> pack path <> " 🐶" -- <- mind the invisible unicode doggo
  contents <- readFile path
  foundExtensions <- Extensions.find path
  case parse foundExtensions path contents of
    Left err                   -> return . Left $ err
    Right (WithoutSubject _)   -> return . Right $ ()
    Right (WithSubject parsed) -> Right <$> doCleaning clean path contents parsed
  where
    msg = "Gnawing on... "

genericInteract :: Parser a -> Cleaner a -> IO ()
genericInteract parse clean = genericInteractWith (mkInteractor parse clean)

genericInteractWith :: Interactor a -> IO ()
genericInteractWith gi = Extensions.find "." >>= Prelude.interact . gi

mkInteractor :: Parser a -> Cleaner a -> Interactor a
mkInteractor parse clean extensions contents = Text.unpack $
  case parse extensions "<interactive>" (Text.pack contents) of
    Left _                                        -> Text.pack contents
    Right (WithoutSubject s)                      -> s
    Right (WithSubject (prefix, subject, suffix)) -> clean prefix subject suffix

doCleaning :: Cleaner a -> FilePath -> Text -> (Text, a, Text) -> IO ()
doCleaning clean path contents (prefix, x, suffix) = do
  void $ writeFile backupPath contents
  let cleaned = clean prefix x suffix
  writeFile tempPath cleaned
  void $ rename tempPath path
  void $ removeFile backupPath
  where
    backupPath = path ++ ".longboye.bak"
    tempPath   = path ++ ".longboye.tmp"
