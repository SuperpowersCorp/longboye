module Longboye.ModuleStatement
       ( ModuleStatement(..)
       , fromDecl
       ) where

import Language.Haskell.Exts ( ExportSpecList
                             , ModuleHead( ModuleHead )
                             , Module( Module )
                             , ModuleName
                             , SrcSpanInfo
                             , WarningText
                             )

data ModuleStatement = ModuleStatement
  { modName           :: ModuleName SrcSpanInfo
  , warningTextMay    :: Maybe (WarningText SrcSpanInfo)
  , exportSpecListMay :: Maybe (ExportSpecList SrcSpanInfo)
  } deriving (Eq, Ord, Show)

fromDecl :: Module SrcSpanInfo -> ModuleStatement
fromDecl decl = ModuleStatement modName' warningTextMay' exportSpecListMay'
  -- Deconstructing `Just` is safe because we can't get this far without a
  -- valid module statement.
  where
    (ModuleHead _ modName' warningTextMay' exportSpecListMay') = modHead
    (Just modHead)                                             = modHeadMay
    (Module _ modHeadMay _ _ _)                                = decl
