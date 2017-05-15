module Longboye.Import
       ( Import(..)
       , from
       , fromDecl
       ) where

import Data.Text                    ( Text )
import Longboye.Member              ( Member )
import Language.Haskell.Exts        ( SrcSpanInfo )
import Language.Haskell.Exts.Syntax ( ImportDecl )

data Import = Import
  { qualified      :: Bool
  , importedModule :: Text
  , asClause       :: Maybe Text
  , hiding         :: Bool
  , members        :: Maybe [Member]
  } deriving (Eq, Ord, Read, Show)

from :: Bool -> Text -> Maybe Text -> Bool -> Maybe [Member] -> Import
from _qualified _importedModule _asClause _hiding _members =
  error "Import.from not implemented."

fromDecl :: ImportDecl SrcSpanInfo -> Import
fromDecl _decl = error "fromDecl not implemented."
