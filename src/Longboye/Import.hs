{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.Import
       ( Import(..)
       , asLength
       , format
       , fromDecl
       ) where

import           Longboye.Prelude

import qualified Data.Text             as Text
import           Language.Haskell.Exts           ( ImportDecl
                                                 , ImportSpecList( ImportSpecList )
                                                 , ModuleName( ModuleName )
                                                 , SrcSpanInfo
                                                 , importAs
                                                 , importModule
                                                 , importQualified
                                                 , importSpecs
                                                 )
import           Longboye.Member                 ( Member )
import qualified Longboye.Member       as Member

data Import = Import
  { qualified      :: Bool
  , importedModule :: Text
  , asClause       :: Maybe Text
  , hiding         :: Bool
  , members        :: Maybe [Member]
  } deriving (Eq, Ord, Read, Show)

fromDecl :: ImportDecl SrcSpanInfo -> Import
fromDecl decl = Import qual modName asC hid membs
  where
    qual    = importQualified decl
    modName = renderModName . importModule $ decl
    asC     = renderModName <$> importAs decl
    hid     = maybe False isHiding (importSpecs decl)
    membs   = Member.fromDecl <$> importSpecs decl

    isHiding (ImportSpecList _ h _) = h

format :: Bool -> Bool -> Int -> Int -> Import -> Text
format anyQual anyHiding maxModLen maxAsLen imp =
  Text.stripEnd
    $ "import "
    <> qual
    <> paddedMod
    <> formattedAs
    <> mHiding
    <> formattedMembs
  where
    paddedMod = pad maxModLen (importedModule imp)
    qual
      | qualified imp = "qualified "
      | anyQual       = "          "
      | otherwise     = ""
    formattedAs    = pad maxAsLenPad $ maybe "" (" as " <>) (asClause imp)
    formattedMembs = formatMembers qual anyHiding maxAsLenPad maxModLen membs
    membs          = members imp
    mHiding
      | isHiding  = " hiding "
      | anyHiding = "        "
      | otherwise = " "
    isHiding    = hiding imp
    maxAsLenPad = maxAsLen + asPad
    asPad       = if maxAsLen == 0 then 0 else 4
    pad n s     = s <> padding
      where
        padding = Text.replicate (n - Text.length s) " "

asLength :: Import -> Int
asLength = maybe 0 Text.length . asClause

formatMembers :: Text -> Bool -> Int -> Int -> Maybe [Member] -> Text
formatMembers qual anyHiding maxAsLen maxModLen = maybe "" f
  where
    f membs = "("
      <> firstPadding
      <> (Text.intercalate sep . map (Member.render sep) . Member.sort $ membs)
      <> lastPadding
      <> ")"
      where
        firstPadding = if null membs then "" else " "
        lastPadding
          | null membs      = ""
          | singleLineMembers = " "
          | otherwise         = "\n" <> padding
        singleLineMembers = length membs == 1 &&
          (maybe 0 Member.opCount . head $ membs) <= 1
    sep     = "\n" <> padding <> ", "
    hideLen = if anyHiding then 7 else 0
    padding = Text.replicate n " "
      where
        n = 1 + maxAsLen + maxModLen + hideLen + Text.length ("import " <> qual)

renderModName :: ModuleName a -> Text
renderModName (ModuleName _ name) = Text.pack name
