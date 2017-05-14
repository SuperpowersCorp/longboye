module Longboye.Member ( Member(..) ) where

import Data.Text ( Text )

data Member
  = ClassMember Text (Maybe [Text])
  | FnMember Text
  | OpMember Text
  deriving (Eq, Ord, Read, Show)
