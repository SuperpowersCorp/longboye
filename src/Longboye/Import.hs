module Longboye.Import ( Import(..), from ) where

import Data.Text       ( Text )
import Longboye.Member ( Member )

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
