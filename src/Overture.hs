{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Overture
       ( (|>)
       , eitherToMaybe
       , getLine
       , headMay
       , newLine
       , putLine
       , show_
       , withDefault
       ) where

import           Prelude                hiding ( getLine )

import           Data.Maybe                    ( fromMaybe )
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

class SchrödingerBox m a where
  alive :: m a -> Bool
  alive = not . dead

  dead  :: m a -> Bool
  dead  = not . alive

  value :: m a -> Maybe a

  {-# MINIMAL (value, alive) | (value, dead )#-}

instance SchrödingerBox Maybe a where
  alive Nothing  = False
  alive _        = True
  value          = id

instance SchrödingerBox (Either a) b where
  alive (Left _) = False
  alive _        = True
  value          = eitherToMaybe

withDefault :: SchrödingerBox m a => a -> m a -> a
withDefault = (. value) . fromMaybe
