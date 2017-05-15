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
import           Longboye.Member                 ( Member )
import qualified Longboye.Member       as Member
import           Language.Haskell.Exts           ( ImportDecl
                                                 , ImportSpecList( ImportSpecList )
                                                 , ModuleName( ModuleName )
                                                 , SrcSpanInfo
                                                 , importAs
                                                 , importModule
                                                 , importQualified
                                                 , importSpecs
                                                 )

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

format :: Int -> Int -> Import -> Text
format maxModLen maxAsLenM4 imp =
  Text.stripEnd $ "import " <> qual <> " " <> paddedMod <> formattedAs <> formattedMembs
  where paddedMod = pad maxModLen (importedModule imp)
        qual      = if qualified imp
                      then "qualified"
                      else "         "
        formattedAs    = pad maxAsLen . maybe "" (" as " <>) . asClause $ imp
        formattedMembs = mHiding <> formatMembers maxAsLen maxModLen (members imp)
        mHiding        = bool "" " hiding " (hiding imp) -- TODO: Fix this
        maxAsLen       = maxAsLenM4 + 4
        pad n s        = s <> padding
          where padding = Text.replicate (n - Text.length s) " "

asLength :: Import -> Int
asLength = fromMaybe 0 . (Text.length <$>) . asClause

formatMembers :: Int -> Int -> Maybe [Member] -> Text
formatMembers maxAsLen maxModLen = maybe "" f
  where f ms    = " ( "
                    <> (Text.intercalate sep . map (Member.render sep) $ ms)
                    <> lastPadding
                    <> ")"
          where lastPadding
                  | null ms        = ""
                  | length ms == 1 = " "
                  | otherwise      = "\n" <> padding
        sep     = "\n" <> padding <> ", "
        padding = Text.replicate n " "
          where n = 1 + maxAsLen + maxModLen + Text.length "import qualified "

renderModName :: ModuleName a -> Text
renderModName (ModuleName _ name) = Text.pack name
