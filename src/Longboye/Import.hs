module Longboye.Import ( Import, from ) where

import Data.Text       ( Text )
import Longboye.Member ( Member )

data Import = Import
  { _qualified     :: Bool
  , importedModule :: Text
  , asClause       :: Maybe Text
  , hiding         :: Bool
  , members        :: Maybe [Member]
  } deriving (Eq, Ord, Read, Show)

from :: Bool -> Text -> Maybe Text -> Bool -> Maybe [Member] -> Import
from qualified importedModule asClause hiding members =
  error "Import.from not implemented."
