{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module LB20.Imports
     ( adjustLocs
     , format
     ) where

import           Longboye.Prelude                    hiding ( get
                                                            , mod
                                                            )

import           Control.Lens                        hiding ( set )
import           Data.Data.Lens                             ( biplate )
import           Data.List                                  ( nub )
import qualified Data.Map.Strict              as Map
import           Data.String                                ( String )
import           Data.Text                                  ( pack )
import           LB20.Lenses
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Prisms

type ImportSummary = Map Fingerprint (ImportDecl SrcSpanInfo)

data Fingerprint = Fingerprint
  { _modNameString :: String
  , _qualified     :: Bool
  , _source        :: Bool
  , _safe          :: Bool
  , _pkgMay        :: Maybe String
  , _asName        :: Maybe String
  , _hiding        :: Bool
  } deriving (Eq, Ord)

format :: Module SrcSpanInfo -> Module SrcSpanInfo
format mod
  | null imports = mod
  | otherwise    = replaceImports imports' mod'
  where
    imports      = extractImports mod
    mod'         = adjustLocs (start, end' - end) mod
    (_, end')    = rangeOf imports'
    imports'     = formatImports start imports
    (start, end) = rangeOf imports

adjustLocs :: (Int, Int) -> Module SrcSpanInfo -> Module SrcSpanInfo
adjustLocs params@(start, _offset) mod@(Module l h p i _) = mod'
  where
    mod' = Module l h p i d'
    Module _ _ _ _ d' = mod
      & ( adjustMod params (srcSpanL . srcSpanEndLineL)
        . adjustMod params (srcSpanL . srcSpanStartLineL)
        . adjustMod params (srcInfoPointsL . traverse . srcSpanStartLineL . filtered (>start))
        )
adjustLocs _ _ = notSupported "XmlHybrid/XmlPage"

adjustMod :: (Int, Int)
          -> Traversal' SrcSpanInfo Int
          -> Module SrcSpanInfo
          -> Module SrcSpanInfo
adjustMod (_start, offset) l m = m
  & ((biplate :: Traversal' (Module SrcSpanInfo) SrcSpanInfo) . l) +~ offset

extractImports :: Module SrcSpanInfo -> [ImportDecl SrcSpanInfo]
extractImports (Module _l _ _ decls _) = decls
extractImports XmlHybrid {}            = notSupported "XmlHybrid"
extractImports XmlPage {}              = notSupported "XmlPage"

formatImports :: Int -> [ImportDecl SrcSpanInfo] -> [ImportDecl SrcSpanInfo]
formatImports start imports
  | null imports = imports
  | otherwise    = rehydrate start importSummary
    where
      importSummary :: ImportSummary
      importSummary = foldl' f Map.empty imports
        where
          f :: Map Fingerprint (ImportDecl SrcSpanInfo)
            -> ImportDecl SrcSpanInfo
            -> Map Fingerprint (ImportDecl SrcSpanInfo)
          f acc imp = Map.insertWith merge (importFingerprint imp) imp acc

          merge :: ImportDecl SrcSpanInfo
                -> ImportDecl SrcSpanInfo
                -> ImportDecl SrcSpanInfo
          merge newImp oldImp = oldImp { importSpecs = specs }
            where
              specs    = mergeSpecs oldSpecs newSpecs

              oldSpecs = importSpecs oldImp
              newSpecs = importSpecs newImp

              mergeSpecs :: Maybe (ImportSpecList SrcSpanInfo)
                         -> Maybe (ImportSpecList SrcSpanInfo)
                         -> Maybe (ImportSpecList SrcSpanInfo)
              mergeSpecs Nothing x         = x
              mergeSpecs x Nothing         = x
              mergeSpecs (Just x) (Just y) = Just $ mergeSpecList x y

              mergeSpecList :: ImportSpecList SrcSpanInfo
                            -> ImportSpecList SrcSpanInfo
                            -> ImportSpecList SrcSpanInfo
              mergeSpecList (ImportSpecList x b s1) (ImportSpecList _ _ s2) =
                ImportSpecList x b (nub . sort $ s1 ++ s2)

rehydrate :: Int -> ImportSummary -> [ImportDecl SrcSpanInfo]
rehydrate start summary = fixSpans start imports'
  where
    imports'         = sortOn nameThenQual . Map.elems $ summary
    nameThenQual imp = (nameText, qual, src, safe, pkg, asName)
      where
        ImportDecl _ modName qual src safe pkg asName _ = imp
        ModuleName _ nameString                         = modName
        nameText                                        = pack nameString

fixSpans :: Int -> [ImportDecl SrcSpanInfo] -> [ImportDecl SrcSpanInfo]
fixSpans delta imps = reverse . foldl f [] $ imps
  where
    f :: [ImportDecl SrcSpanInfo] -> ImportDecl SrcSpanInfo -> [ImportDecl SrcSpanInfo]
    f previous current = fixOne current:previous

    fixOne :: ImportDecl SrcSpanInfo -> ImportDecl SrcSpanInfo
    fixOne imp = imp & (_ImportDecl . _1 . srcSpanL) %~ fixSrcSpan

    fixSrcSpan :: SrcSpan -> SrcSpan
    fixSrcSpan (SrcSpan fn sl sc el ec) = SrcSpan fn (adjust sl) sc (adjust el) ec

    adjust :: Int -> Int
    adjust = (+ delta)

    anyQualified :: Bool
    anyQualified = any importQualified imps

    _qualifiedLength :: Int
    _qualifiedLength = if anyQualified then 11 else 0

    _modNameLength :: Int
    _modNameLength = maximum . map length $ modNameStrings

    modNameStrings :: [String]
    modNameStrings = map (view (_ImportDecl . _2 . _ModuleName . _2)) imps

importFingerprint :: ImportDecl SrcSpanInfo -> Fingerprint
importFingerprint (ImportDecl _ (ModuleName _ mn) q src sf p a specs)
  = Fingerprint mn q src sf p (fmap extractModuleName a) hiding
  where
    extractModuleName (ModuleName _ s) = s

    hiding = case specs of
      Nothing     -> False
      Just specs' -> extractHiding specs'

    extractHiding :: ImportSpecList a -> Bool
    extractHiding (ImportSpecList _ b _) = b

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
