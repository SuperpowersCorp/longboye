{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.ModuleStatements ( cleanText ) where

import           Longboye.Prelude                     hiding ( interact
                                                             , readFile
                                                             , writeFile
                                                             )

import qualified Data.Text                    as Text
import qualified Debug
import           Language.Haskell.Exts                       ( ModuleName( ModuleName )
                                                             , QName( Qual
                                                                    , Special
                                                                    , UnQual
                                                                    )
                                                             )
import           Language.Haskell.Exts.Syntax                ( ExportSpec( EAbs
                                                                         , EModuleContents
                                                                         , EThingWith
                                                                         , EVar
                                                                         )
                                                             , ExportSpecList( ExportSpecList )
                                                             , Name( Ident
                                                                   , Symbol
                                                                   )
                                                             )
import           Longboye.ModuleStatement                    ( ModuleStatement
                                                             , exportSpecListMay
                                                             , modName
                                                             )

cleanText :: Text -> ModuleStatement -> Text -> Text
cleanText prefix moduleStatement suffix =
  formatPrefix (Debug.log "prefix" prefix)
  <> formatModuleStatement moduleStatement
  <> formatSuffix (Debug.log "suffix" suffix)
  where
    formatPrefix s =
      if Text.null s'
        then s'
        else s' <> "\n\n"
      where
        s' = Text.stripEnd s

    formatSuffix s =
      if Text.null s'
        then s'
        else "\n" <> s'
      where
        s' = Text.stripStart s

    formatModuleStatement :: ModuleStatement -> Text
    formatModuleStatement ms = "module "
                                <> fullModuleName
                                <> exportList
                                <> " where\n"
      where
        fullModuleName :: Text
        fullModuleName = Debug.log "fullModuleName" $ case modName ms of
          ModuleName _ n -> Text.pack n

        exportList :: Text
        exportList = case exportSpecListMay ms of
          Nothing         -> "[NO EXPORT LISTS - REMOVE ME]"
          Just (ExportSpecList _ [spec]) ->
            " ( " <> renderExport spec <> " )"
          Just (ExportSpecList _ specs) ->
            (<> multiSuffix)
            . (multiPrefix <>)
            . Text.intercalate "\n     , "
            . map renderExport
            $ specs

        multiPrefix = "\n     ( "
        multiSuffix = "\n     )"

renderExport :: ExportSpec a -> Text
renderExport (EVar _ evar)              = renderEVar evar
renderExport (EAbs _ _ns _qn)           = panic "[EAbs:NOT IMPL]"
renderExport (EThingWith _ _wc _qn _cn) = panic "[EThingWith:NOT IMPL]"
renderExport (EModuleContents _ _mn)    = panic "[EModuleContents:NOT IMPL]"

renderEVar :: QName a -> Text
renderEVar (Qual _ m n) = "[RENDERED_EXPORT:EVar-Qual:m=" <> mm <> ":n=" <> nn <> "]"
  where
    mm = renderModName m
    nn = renderName n
renderEVar (UnQual _ n) = renderName n
renderEVar (Special _ _sc) = panic "[Special not impl!]"

renderModName :: ModuleName a -> Text
renderModName (ModuleName _ n') = "[MODNAME:" <> Text.pack n' <> "]"

renderName :: Name a -> Text
renderName (Ident _ n)  = Text.pack n
renderName (Symbol _ s) = "(" <> Text.pack s <> ")"
