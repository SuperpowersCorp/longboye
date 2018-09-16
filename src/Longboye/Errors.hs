{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.Errors
  ( renderError
  ) where

import Longboye.Prelude             hiding ( SrcLoc
                                           , srcLoc
                                           )

import Language.Haskell.Exts.SrcLoc        ( SrcLoc )

renderError :: SrcLoc -> Text -> Text
renderError srcLoc err = "ERROR at " <> show srcLoc <> ": " <> err
