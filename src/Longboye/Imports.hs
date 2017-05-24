module Longboye.Imports ( clean, interact, interactS ) where

import           Prelude                         hiding ( readFile
                                                        , interact
                                                        , writeFile
                                                        )
import qualified Prelude

import           Control.Monad                          ( foldM
                                                        , void
                                                        )
import           Data.Char                              ( ord )
import           Data.List                              ( isPrefixOf
                                                        , nub
                                                        , sortBy
                                                        )
import           Data.Maybe                             ( fromMaybe )
import           Data.Monoid                            ( (<>) )
import           Data.Ord                               ( comparing )
import           Data.Text                              ( Text
                                                        , unpack
                                                        )
import qualified Data.Text             as Text
import           Data.Text.IO                           ( readFile
                                                        , writeFile
                                                        )
import           Longboye.Import                        ( Import )
import qualified Longboye.Import       as Import
import           Longboye.Parser                        ( Parsed( NoImports
                                                                , WithImports
                                                                ) )
import qualified Longboye.Parser       as Parser
import           System.Directory                       ( listDirectory
                                                        , removeFile
                                                        )
import           System.FilePath.Posix                  ( joinPath )
import           System.Posix.Files                     ( getFileStatus
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
  putStrLn $ cuteMsg ++ "... " ++ path ++ " 🐶"
  contents <- readFile path
  case Parser.parseE path contents of
    Left err                    -> return . Left $ err
    Right (NoImports _)         -> return . Right $ ()
    Right (WithImports parsed) -> Right <$> doCleaning path contents parsed
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

interact :: IO ()
interact = Prelude.interact interactS

interactS :: String -> String
interactS contents = Text.unpack $
  case Parser.parseE "<interactive>" (Text.pack contents) of
    Left _                                        -> Text.pack contents
    Right (NoImports s)                           -> s
    Right (WithImports (prefix, imports, suffix)) -> cleanText prefix imports suffix

doCleaning :: FilePath -> Text -> (Text, [Import], Text) -> IO ()
doCleaning path contents (prefix, imports, suffix) = do
  void $ writeFile backupPath contents
  let cleaned = cleanText prefix imports suffix
  writeFile tempPath cleaned
  void $ rename tempPath path
  void $ removeFile backupPath
  where backupPath = path ++ ".longboye.bak"
        tempPath   = path ++ ".longboye.tmp"

cleanText :: Text -> [Import] -> Text -> Text
cleanText prefix imports suffix =
  formatPrefix prefix <> formatImports finalImports <> formatSuffix suffix
  where formatPrefix  = (<> "\n\n") . Text.stripEnd
        formatSuffix  = (<> "\n") . Text.stripEnd . (suffixSep <>) . Text.stripStart
        suffixSep     = if (Text.null . Text.strip) suffix then "" else "\n"
        formatImports = Text.unlines . sep . map fmt
        fmt           = Import.format anyQual anyHiding maxModLen maxAsLen
        anyQual       = any Import.qualified imports
        anyHiding     = any Import.hiding imports
        maxModLen     = maximum . map (Text.length . Import.importedModule) $ imports
        maxAsLen      = maximum . map Import.asLength                       $ imports
        finalImports  = nub . sortBy (comparing sortDetails) $ imports
        npo           = length . filter isPreludish $ finalImports
        isPreludish   = flip any ["Prelude", "Overture"] . (==) . Import.importedModule
        sep is        = if npo <= 0
                          then is
                          else mconcat [pos, space, rest]
                            where (pos, rest) = splitAt npo is
                                  space       = [""]
        sortDetails i = fromMaybe (im, q) prioritySortValue
                        where
                          prioritySortValue
                            | im == "Prelude"  = Just ("30", q)
                            | im == "Overture" = Just ("60", q)
                            | otherwise  = Nothing
                          im = Import.importedModule i
                          q  = Import.qualified i
