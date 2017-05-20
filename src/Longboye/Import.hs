module Longboye.Import
       ( Import(..)
       , asLength
       , format
       , fromDecl
       ) where

import           Data.Bool                       ( bool )
import           Data.Maybe                      ( fromMaybe )
import           Data.Monoid                     ( (<>) )
import           Data.Text                       ( Text )
import qualified Data.Text             as Text
import           Language.Haskell.Exts           ( ImportDecl
                                                 , ImportSpecList( ImportSpecList)
                                                 , ModuleName( ModuleName)
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
  where qual            = importQualified decl
        modName         = renderModName . importModule $ decl
        asC             = renderModName <$> importAs decl
        hid             = fromMaybe False (isHiding <$> importSpecs decl)
        membs           = Member.fromDecl <$> importSpecs decl

        isHiding (ImportSpecList _ h _) = h

format :: Bool -> Int -> Int -> Import -> Text
format anyQual maxModLen maxAsLen imp =
  Text.stripEnd
    $ "import "
    <> qual
    <> paddedMod
    <> formattedAs
    <> mHiding
    <> formattedMembs
  where paddedMod = pad maxModLen (importedModule imp)
        qual
          | qualified imp = "qualified "
          | anyQual       = "          "
          | otherwise     = ""
        formattedAs    = pad maxAsLenPad $ maybe "" (" as " <>) (asClause imp)
        formattedMembs = formatMembers qual isHiding maxAsLenPad maxModLen membs
        membs          = members imp
        mHiding        = if isHiding then " hiding " else " "
        isHiding       = hiding imp
        maxAsLenPad    = maxAsLen + asPad
        asPad          = if maxAsLen == 0 then 0 else 4
        pad n s        = s <> padding
          where padding = Text.replicate (n - Text.length s) " "

asLength :: Import -> Int
asLength = fromMaybe 0 . (Text.length <$>) . asClause

formatMembers :: Text -> Bool -> Int -> Int -> Maybe [Member] -> Text
formatMembers qual isHiding maxAsLen maxModLen = maybe "" f
  where f ms    = "( "
                    <> (Text.intercalate sep . map (Member.render sep) $ ms)
                    <> lastPadding
                    <> ")"
          where lastPadding
                  | null ms        = ""
                  | length ms == 1 = " "
                  | otherwise      = "\n" <> padding
        sep     = "\n" <> padding <> ", "
        hideLen = bool 0 8 isHiding
        padding = Text.replicate n " "
          where n = maxAsLen + maxModLen + hideLen + Text.length ("import " <> qual)

renderModName :: ModuleName a -> Text
renderModName (ModuleName _ name) = Text.pack name
