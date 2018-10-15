{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Exts.X
  ( module X
  , srcSpanL
  , srcSpanEndLineL
  , srcInfoPointsL
  , srcSpanStartLineL
  ) where

import Longboye.Prelude

import Control.Lens               hiding ( set )
import Language.Haskell.Exts as X

srcSpanL :: Lens' SrcSpanInfo SrcSpan
srcSpanL = lens srcInfoSpan set
  where
    set :: SrcSpanInfo -> SrcSpan -> SrcSpanInfo
    set (SrcSpanInfo _ sips) ss = SrcSpanInfo ss sips

srcSpanEndLineL :: Lens' SrcSpan Int
srcSpanEndLineL = lens srcSpanEndLine set
  where
    set :: SrcSpan -> Int -> SrcSpan
    set (SrcSpan fn sl sc _ ec) el = SrcSpan fn sl sc el ec

srcInfoPointsL :: Lens' SrcSpanInfo [SrcSpan]
srcInfoPointsL = lens srcInfoPoints set
  where
    set :: SrcSpanInfo -> [SrcSpan] -> SrcSpanInfo
    set (SrcSpanInfo x _) y = SrcSpanInfo x y

srcSpanStartLineL :: Lens' SrcSpan Int
srcSpanStartLineL = lens srcSpanStartLine set
  where
    set :: SrcSpan -> Int -> SrcSpan
    set (SrcSpan fn _ sc el ec) sl = SrcSpan fn sl sc el ec
