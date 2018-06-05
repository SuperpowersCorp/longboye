{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LB20.Pragmas ( format ) where

import Longboye.Prelude

import Language.Haskell.Exts

format :: Module SrcSpanInfo -> Module SrcSpanInfo
format = panic "format undefined"
