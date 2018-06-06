{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module LB20.Imports ( format ) where

import Longboye.Prelude      hiding ( get
                                    , mod
                                    )

import Control.Lens          hiding ( set )
import Data.Data.Lens               ( biplate )
import Language.Haskell.Exts
import LB20.Lenses

format :: Module SrcSpanInfo -> Module SrcSpanInfo
format mod
  | null imports = mod
  | otherwise    = replaceImports imports' mod'
  where
    imports      = extractImports mod
    mod'         = adjustLocs (end' - end) mod
    (_, end')    = rangeOf imports'
    imports'     = formatImports (start, end) imports
    (start, end) = rangeOf imports

-- TODO: update srcInfoPoints too?
adjustLocs :: Int -> Module SrcSpanInfo -> Module SrcSpanInfo
adjustLocs offset = adjust offset srcSpanEndLineL . adjust offset srcSpanStartLineL

-- TODO: we need to only touch the appropriate decls...
adjust :: Int -> Lens' SrcSpan Int -> Module SrcSpanInfo -> Module SrcSpanInfo
adjust offset l m = m
  & ((biplate :: Traversal' (Module SrcSpanInfo) SrcSpanInfo) . srcSpanL . l) +~ offset

extractImports :: Module SrcSpanInfo -> [ImportDecl SrcSpanInfo]
extractImports (Module _l _ _ decls _) = decls
extractImports XmlHybrid {}            = notSupported "XmlHybrid"
extractImports XmlPage {}              = notSupported "XmlPage"

formatImports :: (Int, Int) -> [ImportDecl SrcSpanInfo] -> [ImportDecl SrcSpanInfo]
formatImports (_start, _end) imports = imports
  -- TODO: actually format the imports

rangeOf :: [ImportDecl SrcSpanInfo] -> (Int, Int)
rangeOf imports = case head imports of
  Nothing -> panic unpossible
  Just firstImport -> case lastMay imports of
    Nothing -> panic unpossible
    Just lastImport -> (start, end)
      where
        start = srcSpanStartLine . srcInfoSpan . importAnn $ firstImport
        end   = srcSpanEndLine   . srcInfoSpan . importAnn $ lastImport
  where
    unpossible = "rangeOf should only have been called with non-empty imports."

replaceImports :: [ImportDecl SrcSpanInfo] -> Module SrcSpanInfo -> Module SrcSpanInfo
replaceImports imports (Module l h p _ d) = Module l h p imports d
replaceImports _ _                        = notSupported "XmlHybrid/XmlPage"

-- So in theory, what we need to do is:
--
-- 1. Identify the first import
-- 2. Identify the last import.
-- 3. Grab the imports
-- 4. Adjust the positions of all the imports to be laid out right
-- 5. Adjust the positions of all the SrcSpan's after this accordingly.
--
-- Easy, right?
--

-- Is it worth doing something like we are currently doing where we grab
-- "sections" of the document and separate the process of processing the
-- section from the process of re-assembling the sections?
