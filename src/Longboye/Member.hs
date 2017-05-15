module Longboye.Member
       ( Member(..)
       , fromDecl
       ) where

import Data.Text             ( Text )
import Language.Haskell.Exts ( ImportSpecList
                             , SrcSpanInfo
                             )

data Member
  = ClassMember Text (Maybe [Text])
  | FnMember Text
  | OpMember Text
  deriving (Eq, Ord, Read, Show)

fromDecl :: ImportSpecList SrcSpanInfo -> [Member]
fromDecl _ = [FnMember "FIX-ME-FN-MEMBER"]
