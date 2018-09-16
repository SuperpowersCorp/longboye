{-# LANGUAGE NoImplicitPrelude #-}

module Longboye.Common
  ( parseMode
  ) where

import Longboye.Prelude

import Data.List                       ( nub )
import Language.Haskell.Exts           ( ParseMode )
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser    ( baseLanguage
                                       , defaultParseMode
                                       , extensions
                                       , ignoreLanguagePragmas
                                       , parseFilename
                                       )

parseMode :: FilePath -> [Extension] -> ParseMode
parseMode path foundExtensions = defaultParseMode
  { baseLanguage          = Haskell2010
  , ignoreLanguagePragmas = False
  , extensions            = configuredExtensions foundExtensions
  , parseFilename         = path
  }

configuredExtensions :: [Extension] -> [Extension]
configuredExtensions foundExtensions = nub
  $ extensions defaultParseMode
  ++ foundExtensions
  ++ alwaysExtensions

alwaysExtensions :: [Extension]
alwaysExtensions = map EnableExtension
  [ DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PatternSynonyms
  , TypeApplications
  ]
