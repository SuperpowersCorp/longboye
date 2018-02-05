{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.PragmasParser where

import Longboye.Prelude

import Language.Haskell.Exts.Extension ( Extension )
import Longboye.Pragma                 ( Pragma )

data Parsed
  = NoPragmas Text
  | WithPragmas (Text, [Pragma], Text)
  deriving (Eq, Ord, Read, Show)

parse :: [Extension] -> FilePath -> Text -> Maybe Parsed
parse foundExtensions path = eitherToMaybe . parseE foundExtensions path

parseE :: [Extension] -> FilePath -> Text -> Either Text Parsed
parseE _foundExtensions _path _source = Left "PragmasParser.parseE not impl"
