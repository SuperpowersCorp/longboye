{-# LANGUAGE NoImplicitPrelude #-}

module LB20.Lenses
     ( srcSpanL
     , srcSpanEndLineL
     , srcSpanStartLineL
     ) where

import Longboye.Prelude

import Control.Lens          hiding ( set )
import Language.Haskell.Exts

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

srcSpanStartLineL :: Lens' SrcSpan Int
srcSpanStartLineL = lens srcSpanStartLine set
  where
    set :: SrcSpan -> Int -> SrcSpan
    set (SrcSpan fn sl sc _ ec) el = SrcSpan fn sl sc el ec
