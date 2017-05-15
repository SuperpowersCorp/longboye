module Longboye.Import
       ( Import(..)
       , asLength
       , format
       , fromDecl
       ) where

import           Data.Maybe                   ( fromMaybe )
import           Data.Monoid                  ( (<>) )
import           Data.Text                    ( Text )
import qualified Data.Text                    as Text
import           Longboye.Member              ( Member )
import qualified Longboye.Member              as Member
import           Language.Haskell.Exts        ( ImportDecl
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
        asC             = renderModName <$> (importAs decl)
        hid             = error "hid not implemented."
        membs           = Member.fromDecl <$> importSpecs decl

format :: Int -> Int -> Import -> Text
format maxModLen maxAsLen imp =
  "import " <> qual <> " " <> paddedMod <> formattedAs <> formattedMembs
  where paddedMod = pad maxModLen (importedModule imp)
        qual      = if qualified imp
                      then "qualified"
                      else "         "
        formattedAs    = fromMaybe "" $ (pad maxAsLen) <$> asClause imp
        formattedMembs = formatMembers (members imp)
        pad n s   = s <> padding
          where padding = Text.replicate ((Text.length s) - n) " "

asLength :: Import -> Int
asLength = fromMaybe 0 . (Text.length <$>) . asClause

formatMembers :: Maybe [Member] -> Text
formatMembers Nothing         = ""
formatMembers (Just _members) = " ( " <> memberList <> " )"
  where memberList = "MEMBERS SOON" -- TODO: finish me

renderModName :: ModuleName a -> Text
renderModName (ModuleName _ name) = Text.pack name
