{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.Pragmas ( clean, interact, interactS ) where

import qualified Prelude
import           Longboye.Prelude                              hiding ( interact )

import qualified Data.Text                       as Text
import           Language.Haskell.Exts                                ( ModulePragma( AnnModulePragma
                                                                                    , LanguagePragma
                                                                                    , OptionsPragma
                                                                                    )
                                                                      , Name( Ident
                                                                            , Symbol
                                                                            )
                                                                      , SrcSpanInfo
                                                                      , Tool( UnknownTool )
                                                                      )
import           Language.Haskell.Exts.Extension                      ( Extension )
import qualified Longboye.Extensions             as Extensions
import           Longboye.PragmasParser                               ( Parsed( NoPragmas
                                                                              , WithPragmas
                                                                              )
                                                                      , Pragma
                                                                      )
import qualified Longboye.PragmasParser          as Parser
import           System.Directory                                     ( listDirectory
                                                                      , removeFile
                                                                      )
import           System.FilePath.Posix                                ( joinPath )
import           System.Posix.Files                                   ( getFileStatus
                                                                      , isDirectory
                                                                      , rename
                                                                      )

-- TODO: DRY up these clean* function vs eg Imports
clean :: [FilePath] -> IO ()
clean []           = return ()
clean (path:paths) = cleanPath path >>= either abort continue
  where
    abort err = panic $ "An error occured: " <> err
    continue  = const $ clean paths

cleanPath :: FilePath -> IO (Either Text ())
cleanPath path = do
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
  putLn $ msg <> Text.pack path <> " 🐶" -- <- mind the invisible unicode doggo
  contents <- readFile path
  foundExtensions <- Extensions.find path
  case Parser.parseE foundExtensions path contents of
    Left err                   -> return . Left $ err
    Right (NoPragmas _)        -> return . Right $ ()
    Right (WithPragmas parsed) -> Right <$> doCleaning path contents parsed
  where
    msg = "Gnawing on... "

doCleaning :: FilePath -> Text -> (Text, [Pragma], Text) -> IO ()
doCleaning path contents (prefix, pragmas, suffix) = do
  void $ writeFile backupPath contents
  let cleaned = cleanText prefix pragmas suffix
  writeFile tempPath cleaned
  void $ rename tempPath path
  void $ removeFile backupPath
  where
    backupPath = path ++ ".longboye.bak"
    tempPath   = path ++ ".longboye.tmp"

interact :: IO ()
interact = Extensions.find "." >>= Prelude.interact . interactS

interactS :: [Extension] -> Prelude.String -> Prelude.String
interactS extensions contents = Text.unpack $
  case Parser.parseE extensions "<interactive>" (Text.pack contents) of
    Left _                                        -> Text.pack contents
    Right (NoPragmas s)                           -> s
    Right (WithPragmas (prefix, pragmas, suffix)) -> cleanText prefix pragmas suffix

-- TODO: Dry up vs eg Imports.cleanText
cleanText :: Text -> [Pragma] -> Text -> Text
cleanText prefix pragmas suffix =
  formatPrefix prefix <> formatPragmas finalPragmas <> formatSuffix suffix
  where
    finalPragmas = sortPragmas . separatePragmas $ pragmas

    formatPragmas :: [Pragma] -> Text
    formatPragmas = Text.unlines . map (fmt padding)

    padding = (subtract 8) . maximum . map (Text.length . (fmt 0)) $ finalPragmas

    fmt :: Int -> Pragma -> Text
    fmt pad (LanguagePragma _ [Ident _ name]) = "{-# LANGUAGE " <> stripPackPad pad 9 name <> " #-}"
    fmt pad (OptionsPragma _ Nothing s)       = "{-# OPTIONS " <> stripPackPad pad 8 s <> " #-}"
    fmt pad (OptionsPragma _ (Just tool) s)   = "{-# OPTIONS_" <> show tool <> " " <> stripPackPad pad (9 + Text.length (show tool)) s <> " #-}"
    fmt _ (AnnModulePragma _ _ann)            = panic "AnnModulePragma not impl yet"
    fmt _ other                               = panic "unimplemented pragma def: " <> show other

    stripPackPad :: Int -> Int -> Prelude.String -> Text
    stripPackPad padTo used = padText (padTo - used) . Text.strip . Text.pack

    padText :: Int -> Text -> Text
    padText n s = s <> Text.pack (replicate (n - Text.length s) ' ')

    -- TODO:
    formatPrefix  = identity
    formatSuffix  = identity

separatePragmas :: [Pragma] -> [Pragma]
separatePragmas = concatMap splitPragma

splitPragma :: Pragma -> [Pragma]
splitPragma pragma@(OptionsPragma _ _ _) = [pragma]
splitPragma pragma@(AnnModulePragma _ _) = [pragma]
splitPragma (LanguagePragma x idents) = map reassemble idents
  where
    reassemble :: Name SrcSpanInfo -> Pragma
    reassemble name = (LanguagePragma x [name])

sortPragmas :: [Pragma] -> [Pragma]
sortPragmas = sortBy (comparing pragConstructor <> comparing pragName)
  where
    pragConstructor :: Pragma -> Int
    pragConstructor (LanguagePragma _ _)  = 0
    pragConstructor (OptionsPragma _ _ _) = 1
    pragConstructor (AnnModulePragma _ _) = panic "unexpected ANN pragma in module front matter"

    pragName :: Pragma -> (Prelude.String, Prelude.String)
    pragName (LanguagePragma _ [Ident _ name])  = ("", name)
    pragName (LanguagePragma _ [Symbol _ name]) = ("", name)
    pragName (LanguagePragma _ _names)          = panic "multiple names found in a single language pragma after splitting step."
    pragName (OptionsPragma _ Nothing name)     = ("", name)
    pragName (OptionsPragma _ (Just tool) name) = (toolName tool, name)
    pragName (AnnModulePragma _ _)              = panic "unexpected ANN pragma in module front matter"

    toolName (UnknownTool s) = s
    toolName other           = show other
