module Longboye.Parsers ( parse ) where

import Text.Megaparsec.Text ()

import qualified Text.Megaparsec as P

parse :: String -> P.Parsec e s a -> s -> Either (P.ParseError (P.Token s) e) a
parse = flip P.parse
