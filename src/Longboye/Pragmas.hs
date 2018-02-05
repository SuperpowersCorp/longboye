{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Longboye.Pragmas ( interact, interactS ) where

import qualified Prelude
import           Longboye.Prelude                              hiding ( interact )

import qualified Data.Text                       as Text
import           Language.Haskell.Exts.Extension                      ( Extension )
import qualified Longboye.Extensions             as Extensions
import           Longboye.Pragma                                      ( Pragma )
import           Longboye.PragmasParser                               ( Parsed( NoPragmas
                                                                              , WithPragmas
                                                                              )
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

cleanText :: Text -> [Pragma] -> Text -> Text
cleanText = panic "Pragmas.cleanText Right not impl!"
