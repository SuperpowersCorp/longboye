module Longboye.Import
       ( Import(..)
       , format
       , fromDecl
       ) where

import           Data.Monoid                  ( (<>) )
import           Data.Text                    ( Text )
import qualified Data.Text                    as Text
import           Longboye.Member              ( Member )
import           Language.Haskell.Exts        ( ImportDecl
                                              , ModuleName( ModuleName )
                                              , SrcSpanInfo
                                              , importQualified
                                              , importModule
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

        asC             = error "asC not implemented."
        hid             = error "hid not implemented."
        membs           = error "membs not implemented."

        renderModName (ModuleName _ name) = Text.pack name

format :: Import -> Text
format imp = "import " <> qual <> " " <> importedModule imp <> rest
  where qual = if qualified imp
                 then "qualified"
                 else "         "
        rest = " [REST COMING SOON]"
