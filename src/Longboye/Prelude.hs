{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Longboye.Prelude
       ( module Exports
       , (|>)
       , eitherToMaybe
       , getLn
       -- , newLine
       , putLn
       -- , show_
       , withDefault
       ) where

import Protolude as Exports

-- import           Prelude                hiding ( getLine )

-- import           Data.Maybe                    ( fromMaybe )
-- import           Data.Text                     ( Text )
-- import qualified Data.Text    as Text
-- import qualified Data.Text.IO as TextIO

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

getLn :: MonadIO m => m Text
getLn = liftIO getLine

-- newLine :: IO ()
-- newLine = putLn ""

putLn :: MonadIO m => Text -> m ()
putLn = putStrLn

-- show_ :: Show a => a -> Text
-- show_ = Text.pack . show

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

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
  value          = identity

instance SchrödingerBox (Either a) b where
  alive (Left _) = False
  alive _        = True
  value          = eitherToMaybe

withDefault :: SchrödingerBox m a => a -> m a -> a
withDefault = (. value) . fromMaybe
