module Longboye.ModuleStatement
       ( ModuleStatement(..)
       ) where

data ModuleStatement = ModuleStatement
  { details :: [String] -- TODO: Fixme
  } deriving (Eq, Ord, Read, Show)
