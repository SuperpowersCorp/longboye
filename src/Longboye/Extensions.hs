{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.Extensions
  ( find
  )  where

import           Longboye.Prelude                                hiding ( find )

import           Data.List                                              ( elemIndices
                                                                        , isSuffixOf
                                                                        , nub
                                                                        )
import qualified Data.Map.Strict                       as Map
import           Data.Text                                              ( pack )
import qualified Distribution.PackageDescription       as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Verbosity                as Cabal
import qualified Language.Haskell.Extension            as Cabal
import qualified Language.Haskell.Exts.Extension       as Source
import           Safe                                                   ( lastMay )
import           System.Directory                                       ( canonicalizePath
                                                                        , doesDirectoryExist
                                                                        , doesFileExist
                                                                        , listDirectory
                                                                        )
import           System.FilePath.Posix                                  ( joinPath
                                                                        , pathSeparator
                                                                        )

-- TODO: more comprehensive search for/testing of extension sets, eg. handling
--       multiple candidate .cabal files being found, etc.

find :: FilePath -> IO [Source.Extension]
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

  where
    handleDir parent p =
      map (addParent p)
        . filter (".cabal" `isSuffixOf`)
        <$> listDirectory p >>= \files ->
          if (not . null) files
            then return files
            else if parent == p
                   then return []
                   else findCandidates parent
    addParent p f   = joinPath [p, f]
    handleFile      = findCandidates . containingDir
    -- this assumes p has been canonicalized by the time it gets to containingDir
    -- TODO: deal with `pathSeparators` instead of just `pathSeparator`
    containingDir p = p
      |> elemIndices pathSeparator
      |> lastMay
      |> withDefault (length p)
      |> flip take p
    doesNotExist    = panic $ "The path '" <> pack inPath <> "' could not be found."

readAllExtensions :: [FilePath] -> IO [Source.Extension]
readAllExtensions = (concat <$>) . mapM readExtensions

readExtensions :: FilePath -> IO [Source.Extension]
readExtensions path = unsafeReadExtensions path `catch` handler
  where
    handler :: SomeException -> IO [Source.Extension]
    handler _ = return []

unsafeReadExtensions :: FilePath -> IO [Source.Extension]
unsafeReadExtensions path = do
  genDesc <- Cabal.readPackageDescription Cabal.silent path
  let buildInfos = allBuildInfoForReal genDesc
      extensions = nub . mconcat . map Cabal.allExtensions $ buildInfos
  -- putStrLn $ "genDesc: " ++ show genDesc
  -- putStrLn $ "desc: " ++ show desc
  -- putStrLn $ "buildInfos: " ++ show buildInfos
  -- putStrLn $ "extensions: " ++ show extensions
  return . mapMaybe extToExt $ extensions
  where
    extToExt :: Cabal.Extension -> Maybe Source.Extension
    extToExt cabalExt = Map.lookup (show cabalExt) sourceExts

sourceExts :: Map Text Source.Extension
sourceExts =
  foldr ((Map.insert =<< show) . Source.EnableExtension) Map.empty knownExtensions
  where
    firstKnownExt = Source.OverlappingInstances
    knownExtensions = [firstKnownExt..]

-- the built in allBuildInfo was behaving as `const []` for some reason, so
-- we roll our own
allBuildInfoForReal :: Cabal.GenericPackageDescription -> [Cabal.BuildInfo]
allBuildInfoForReal genDesc =
  genDesc
    |> Cabal.condLibrary
    |> fmap (return . Cabal.libBuildInfo . Cabal.condTreeData)
    |> withDefault []
