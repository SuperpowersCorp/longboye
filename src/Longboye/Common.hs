{-# LANGUAGE NoImplicitPrelude #-}

module Longboye.Common
  ( parseMode
  ) where

import Longboye.Prelude hiding (Fixity)

import Data.List                       ( nub )
import Language.Haskell.Exts           ( Fixity
                                       , ParseMode
                                       , infixl_
                                       , infixr_
                                       , infix_
                                       )
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser    ( baseLanguage
                                       , defaultParseMode
                                       , extensions
                                       , fixities
                                       , ignoreLanguagePragmas
                                       , parseFilename
                                       )

parseMode :: FilePath -> [Extension] -> ParseMode
parseMode path foundExtensions = defaultParseMode
  { baseLanguage          = Haskell2010
  , extensions            = configuredExtensions foundExtensions
  , fixities              = lensFixities
  , ignoreLanguagePragmas = False
  , parseFilename         = path
  }

lensFixities :: Maybe [Fixity]
lensFixities = Just
  $  infixl_ 9 [ ":>", "<.>", "<.", ".>", "...", "#." ]
  <> infixl_ 8 [ "^..", "^?", "^?!", "^@..", "^@?", "^@?!", "^.", "^@.", "^#"
               , ".#", "^!", "^@!"
               ]
  <> infixr_ 4 [ "</>~", "<</>~", "<.>~", "<<.>~", "<#~", "#~", "#%~", "<#%~"
               , "#%%~", ".|.~", ".&.~", "<.|.~", "<.&.~", "%@~", ".~", "+~"
               , "*~", "-~", "//~", "^~", "^^~", "**~", "&&~", "<>~", "||~"
               , "%~", "%%@~", "<%@~", "%%~", "<+~", "<*~", "<-~", "<//~"
               , "<^~", "<^^~", "<**~" ]
  <> infix_ 4  [ "</>=", "<</>=", "<.>=", "<<.>=", "<#=", "#=", "#%=", "<#%="
               , "#%%=", ".|.=", ".&.=", "<.|.=", "<.&.=", "%@=", ".=", "+="
               , "*=", "-=", "//=", "^=", "^^=", "**=", "&&=", "<>=", "||="
               , "%=", "%%@=", "<%@=", "%%=", "<+=", "<*=", "<-=", "<//="
               , "<^=", "<^^=", "<**="
               ]
  <> infixr_ 2 [ "`zoom`", "`magnify`", "<~", "<<~" ]
  <> infixl_ 1 ["&", "&~", "<&>", "??"]

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
