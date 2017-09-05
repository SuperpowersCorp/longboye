{-# LANGUAGE OverloadedStrings #-}
module Longboye.ModuleStatements ( clean, interact ) where

import           Prelude                                       hiding ( interact
                                                                      , readFile
                                                                      , writeFile
                                                                      )
import qualified Prelude
import           Overture

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
import           Language.Haskell.Exts                                ( ModuleName( ModuleName )
                                                                      , QName( Qual
                                                                             , UnQual
                                                                             , Special
                                                                             )
                                                                      )
import           Language.Haskell.Exts.Extension                      ( Extension )
import           Language.Haskell.Exts.Syntax                         ( ExportSpec( EVar
                                                                                  , EAbs
                                                                                  , EThingWith
                                                                                  , EModuleContents
                                                                                  )
                                                                      , ExportSpecList( ExportSpecList )
                                                                      , Name( Ident
                                                                            , Symbol
                                                                            )
                                                                      )
import qualified Longboye.Extensions             as Extensions
import           Longboye.ModuleStatement                             ( ModuleStatement
                                                                      , exportSpecListMay
                                                                      , modName
                                                                      )
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
  where
    abort err  = error $ "An error occured: " ++ unpack err
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
  where
    f (Right ()) file = cleanPath (joinPath [path, file])
    f err _           = return err

    hidden            = ("." `isPrefixOf`)

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
  where
    backupPath = path ++ ".longboye.bak"
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
    formatPrefix s =
      if Text.null s'
        then s'
        else s' <> "\n"
      where
        s' = Text.stripEnd s

    formatSuffix s = "\n" <> s

    formatModuleStatement :: ModuleStatement -> Text
    formatModuleStatement ms = ("module "
                                <> fullModuleName
                                <> exportList
                                <> " where\n"
                                <> "\n----------------------------------------------------------------\n"
                                <> debugModuleStatement ms
                               )
      where
        fullModuleName :: Text
        fullModuleName =
          case modName ms of
            ModuleName _ n -> Text.pack n

        exportList :: Text
        exportList =
          case exportSpecListMay ms of
            Nothing         -> "[NO EXPORT LISTS - REMOVE ME]"
            Just (ExportSpecList _ [spec]) ->
              " ( " <> renderExport spec <> " )"
            Just (ExportSpecList _ specs) ->
              (<> multiSuffix)
              . (multiPrefix <>)
              . Text.intercalate "\n       , "
              . map renderExport
              $ specs

        multiPrefix = "\n       ( "
        multiSuffix = "\n       )"

renderExport :: ExportSpec a -> Text
renderExport (EVar _ evar)              = renderEVar evar
renderExport (EAbs _ _ns _qn)           = error "[EAbs:NOT IMPL]"
renderExport (EThingWith _ _wc _qn _cn) = error "[EThingWith:NOT IMPL]"
renderExport (EModuleContents _ _mn)    = error "[EModuleContents:NOT IMPL]"

renderEVar :: QName a -> Text
renderEVar (Qual _ m n) =
  "[RENDERED_EXPORT:EVar-Qual:m=" <> mm <> ":n=" <> nn <> "]"
  where
    mm = renderModName m
    nn = renderName n
renderEVar (UnQual _ n) = renderName n
renderEVar (Special _ _sc) = error "[Special not impl!]"

renderModName :: ModuleName a -> Text
renderModName (ModuleName _ n') = "[MODNAME:" <> Text.pack n' <> "]"

renderName :: Name a -> Text
renderName (Ident _ n)  = Text.pack n
renderName (Symbol _ s) = "(" <> Text.pack s <> ")"

debugModuleStatement :: Show a => a -> Text
debugModuleStatement ms = "[[[" <> show_ ms <> "]]]"
