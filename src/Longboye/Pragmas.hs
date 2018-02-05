{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.Pragmas ( interact, interactS ) where

import qualified Prelude
import           Longboye.Prelude                              hiding ( interact )

import qualified Data.Text                       as Text
import qualified Debug
import           Language.Haskell.Exts                                ( ModulePragma( AnnModulePragma
                                                                                    , LanguagePragma
                                                                                    , OptionsPragma
                                                                                    )
                                                                      , Name( Ident )
                                                                      , SrcSpanInfo
                                                                      )
import           Language.Haskell.Exts.Extension                      ( Extension )
import qualified Longboye.Extensions             as Extensions
import           Longboye.PragmasParser                               ( Parsed( NoPragmas
                                                                              , WithPragmas
                                                                              )
                                                                      , Pragma
                                                                      )
import qualified Longboye.PragmasParser          as Parser

interact :: IO ()
interact = Extensions.find "." >>= Prelude.interact . interactS

interactS :: [Extension] -> Prelude.String -> Prelude.String
interactS extensions contents = Text.unpack $
  case Parser.parseE extensions "<interactive>" (Text.pack contents) of
    Left _                                        -> Text.pack contents
    Right (NoPragmas s)                           -> s
    Right (WithPragmas (prefix, pragmas, suffix)) -> cleanText prefix pragmas suffix

-- TODO: Dry up vs eg Imports.cleanText
cleanText :: Text -> [Pragma] -> Text -> Text
cleanText prefix pragmas suffix =
  formatPrefix prefix <> formatPragmas finalPragmas <> formatSuffix suffix
  where
    finalPragmas  = Debug.log "final pragmas" $ separatePragmas (Debug.log "incoming pragmas" pragmas)

    -- TODO: sort within groups

    formatPragmas :: [Pragma] -> Text
    formatPragmas = Text.unlines . map fmt

    fmt :: Pragma -> Text
    fmt (LanguagePragma _ [Ident _ name]) = "{-# LANGUAGE " <> Text.pack name <> " #-}"
    fmt (OptionsPragma _ Nothing s)       = "{-# OPTIONS " <> Text.pack s <> " #-}"
    fmt (OptionsPragma _ (Just tool) s)   = "{-# OPTIONS_" <> show tool <> " " <> Text.pack s <> " #-}"
    fmt (AnnModulePragma _ _ann)          = panic "AnnModulePragma not impl yet"
    fmt other = panic "unimplemented pragma def: " <> show other

    -- TODO:
    formatPrefix  = identity
    formatSuffix  = identity

separatePragmas :: [Pragma] -> [Pragma]
separatePragmas = concatMap splitPragma

splitPragma :: Pragma -> [Pragma]
splitPragma pragma@(OptionsPragma _ _ _) = [pragma]
splitPragma pragma@(AnnModulePragma _ _) = [pragma]
splitPragma (LanguagePragma x idents) = map reassemble idents
  where
    reassemble :: Name SrcSpanInfo -> Pragma
    reassemble name = (LanguagePragma x [name])
