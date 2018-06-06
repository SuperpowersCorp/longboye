{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module LB20.Imports ( adjustLocs, format ) where

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
    mod'         = adjustLocs (adjustStart, end' - end) mod
    (_, end')    = rangeOf imports'
    imports'     = formatImports (start, end) imports
    (start, end) = rangeOf imports
    adjustStart  = panic "adjustStart not impl"

-- TODO: Do we need to update srcInfoPoints too?
-- TODO: we could just pass in the starting line to adjust
adjustLocs :: (Int, Int) -> Module SrcSpanInfo -> Module SrcSpanInfo
adjustLocs params = adjust params srcSpanEndLineL . adjust params srcSpanStartLineL

-- TODO: should be able to restrict the traversal to one that is filtered
--       to just those with srcSpanStartLine > start
adjust :: (Int, Int) -> Lens' SrcSpan Int -> Module SrcSpanInfo -> Module SrcSpanInfo
adjust (_start, offset) l m = m
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
