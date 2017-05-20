module Longboye.Imports ( clean ) where

import           Prelude                             hiding  ( readFile
                                                             , writeFile
                                                             )

import           Control.Monad                       ( void )
import           Data.List                           ( sortBy )
import           Data.Maybe                          ( fromMaybe )
import           Data.Monoid                         ( (<>) )
import           Data.Ord                            ( comparing )
import           Data.Text                           ( Text
                                                     , unpack
                                                     )
import qualified Data.Text                as Text
import           Data.Text.IO                        ( readFile
                                                     , writeFile
                                                     )
import           Longboye.Import                     ( Import )
import qualified Longboye.Import          as Import
import           Longboye.Imports.Cracker            ( Cracked( NoImports
                                                              , WithImports
                                                              ) )
import qualified Longboye.Imports.Cracker as Cracker
import           Longboye.Imports.Verify             ( VerifiedTempPath )
import qualified Longboye.Imports.Verify  as Verify
import           System.Directory                    ( removeFile )
import           System.Posix.Files                  ( rename )

clean :: [FilePath] -> IO ()
clean []           = return ()
clean (path:paths) = cleanFile path >>= either abort continue
  where abort err  = error $ "An error occured: " ++ unpack err
        continue   = const $ clean paths

cleanFile :: FilePath -> IO (Either Text ())
cleanFile path = do
  putStrLn $ "Processing file: " ++ path
  contents <- readFile path
  case Cracker.crackE path contents of
    Left err                    -> return . Left $ err
    Right (NoImports _)         -> return . Right $ ()
    Right (WithImports cracked) -> Right <$> doCleaning path contents cracked

doCleaning :: FilePath -> Text -> (Text, [Import], Text) -> IO ()
doCleaning path contents (prefix, imports, suffix) = do
  void $ writeFile backupPath contents
  let cleaned = cleanText prefix imports suffix
  writeFile tempPath cleaned
  verifiedTempPath <- Verify.tempContent contents tempPath
  void $ swap verifiedTempPath path
  void $ Verify.newContent cleaned path
  void $ removeFile backupPath
  where backupPath = path ++ ".lbak"
        tempPath   = path ++ ".ltemp"

swap :: VerifiedTempPath -> FilePath -> IO ()
swap vtp path = rename src dst
  where src = Verify.accessPath vtp
        dst = path

cleanText :: Text -> [Import] -> Text -> Text
cleanText prefix imports suffix =
  formatPrefix prefix <> formatImports finalImports <> formatSuffix suffix
  where formatPrefix  = (<> "\n\n") . Text.stripEnd
        formatSuffix  = ("\n" <>)   . Text.stripStart
        formatImports = Text.unlines . sep . map fmt
        fmt           = Import.format anyQual maxModLen maxAsLen
        anyQual       = any Import.qualified imports
        maxModLen     = maximum . map (Text.length . Import.importedModule) $ imports
        maxAsLen      = maximum . map Import.asLength                       $ imports
        finalImports  = sortBy (comparing sortDetails) imports
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
