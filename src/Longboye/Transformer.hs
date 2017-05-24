{-# LANGUAGE OverloadedStrings #-}
module Longboye.Transformer
       ( Transformer
       , Context( path
                , contents
                , prefix
                , imports
                , suffix
                )
       , context
       ) where

import Data.Text       ( Text )
import Longboye.Import ( Import )

type Transformer = Context -> Text

data Context = TransformerContext
  { path     :: FilePath
  , contents :: Text
  , prefix   :: Text
  , imports  :: [Import]
  , suffix   :: Text
  } deriving (Eq, Ord, Read, Show)

context :: FilePath -> Text -> Text -> [Import] -> Text -> Context
context = TransformerContext
