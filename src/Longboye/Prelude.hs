{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Longboye.Prelude
       ( module Exports
       , (|>)
       , eitherToMaybe
       , getLn
       , notSupported
       , putLn
       , withDefault
       ) where

import Protolude as Exports

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

getLn :: MonadIO m => m Text
getLn = liftIO getLine

putLn :: MonadIO m => Text -> m ()
putLn = putStrLn

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

notSupported :: Text -> a
notSupported = panic . (<> " not supported.")
