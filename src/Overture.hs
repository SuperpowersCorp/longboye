module Overture
       ( (|>)
       , eitherToMaybe
       , getLine
       , headMay
       , newLine
       , putLine
       , show_
       ) where

import           Prelude                hiding ( getLine )

import           Data.Text                     ( Text )
import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO
import           Safe                          ( headMay )

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

getLine :: IO Text
getLine = TextIO.getLine

newLine :: IO ()
newLine = putLine ""

putLine :: Text -> IO ()
putLine = TextIO.putStrLn

show_ :: Show a => a -> Text
show_ = Text.pack . show

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a
