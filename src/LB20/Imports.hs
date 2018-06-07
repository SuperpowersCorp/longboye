{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module LB20.Imports ( adjustLocs, format ) where

import Longboye.Prelude      hiding ( get
                                    , mod
                                    )

import Control.Lens          hiding ( set )
import Data.Data.Lens               ( biplate )
import LB20.Lenses
import Language.Haskell.Exts

format :: Module SrcSpanInfo -> Module SrcSpanInfo
format mod
  | null imports = mod
  | otherwise    = replaceImports imports' mod'
  where
    imports      = extractImports mod
    mod'         = adjustLocs (start, end' - end) mod
    (_, end')    = rangeOf imports'
    imports'     = formatImports (start, end) imports
    (start, end) = rangeOf imports

adjustLocs :: (Int, Int) -> Module SrcSpanInfo -> Module SrcSpanInfo
adjustLocs params@(start, _offset) mod@(Module l h p i _) = mod'
  where
    mod' = Module l' h' p' i' d'
    l' = l
    -- l' = l & (srcSpanL . srcSpanEndLineL) +~ offset

    h' = h
    p' = p
    i' = i

    Module _ _ _ _ d' = mod
      & ( adjust params (srcSpanL . srcSpanEndLineL)
        . adjust params (srcSpanL . srcSpanStartLineL)
        . adjust params (srcInfoPointsL . traverse . srcSpanStartLineL . filtered (>start))
        )

adjustLocs _ _ = panic "XmlHybrid/XmlPage not supported"

adjust :: (Int, Int)
       -> Traversal' SrcSpanInfo Int
       -> Module SrcSpanInfo
       -> Module SrcSpanInfo
adjust (_start, offset) l m = m
  & ((biplate :: Traversal' (Module SrcSpanInfo) SrcSpanInfo) . l) +~ offset

extractImports :: Module SrcSpanInfo -> [ImportDecl SrcSpanInfo]
extractImports (Module _l _ _ decls _) = decls
extractImports XmlHybrid {}            = notSupported "XmlHybrid"
extractImports XmlPage {}              = notSupported "XmlPage"

formatImports :: (Int, Int)
              -> [ImportDecl SrcSpanInfo]
              -> [ImportDecl SrcSpanInfo]
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
