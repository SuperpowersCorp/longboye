module Longboye.Extensions ( find )  where

import           Data.List                                    ( isSuffixOf
                                                              , nub
                                                              )
import qualified Data.Map.Strict                       as Map
import           Data.Maybe                                   ( catMaybes )
import           Distribution.PackageDescription              ( allBuildInfo
                                                              , allExtensions
                                                              , packageDescription
                                                              )
import           Distribution.PackageDescription.Parse        ( readPackageDescription )
import           Distribution.Verbosity                       ( silent )
import           Language.Haskell.Exts.Extension              ( Extension )
import           System.Directory                             ( canonicalizePath
                                                              , doesDirectoryExist
                                                              , doesFileExist
                                                              , listDirectory
                                                              )
import           System.FilePath.Posix                        ( joinPath )

-- TODO: memoize

-- TODO: more comprehensive search for/testing of extension sets, eg. handling
--       multiple candidate .cabal files being found, etc.

find :: FilePath -> IO [Extension]
find = (readAllExtensions =<<) . findCandidates

findCandidates :: FilePath -> IO [FilePath]
findCandidates inPath = do
  path       <- canonicalizePath inPath
  parentDir  <- canonicalizePath (joinPath [path, ".."])
  dirExists  <- doesDirectoryExist path
  fileExists <- doesFileExist path

  if dirExists
     then handleDir parentDir path
     else if fileExists
            then handleFile path
            else doesNotExist path

  where handleDir parent p =
          map (addParent p)
            . filter (".cabal" `isSuffixOf`)
            <$> listDirectory p >>= \files ->
              if (not . null) files
                then return files
                else if parent == p
                       then return []
                       else findCandidates parent
        addParent p f    = joinPath [p, f]
        handleFile       = (findCandidates =<<) . containingDir
        containingDir _p = undefined
        doesNotExist     = error $ "The path '" ++ inPath ++ "' could not be found."

readAllExtensions :: [FilePath] -> IO [Extension]
readAllExtensions = (concat <$>) . mapM readExtensions

readExtensions :: FilePath -> IO [Extension]
readExtensions path = do
  genDesc <- readPackageDescription silent path
  let desc       = packageDescription genDesc
      buildInfos = allBuildInfo desc
      extensions = nub . mconcat . map allExtensions $ buildInfos
  putStrLn $ "desc: " ++ show desc
  putStrLn $ "buildInfos: " ++ show buildInfos
  putStrLn $ "extensions: " ++ show extensions
  catMaybes <$> mapM extToExt extensions
  where extToExt cabalExt = Map.lookup (toString cabalExt) sourceExts
