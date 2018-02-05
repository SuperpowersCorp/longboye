{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Debug
       ( crash
       , log
       ) where

import Longboye.Prelude hiding ( log )
import System.IO.Unsafe        ( unsafePerformIO )

crash :: Text -> a
crash msg = panic $ "Debug.crash: " <> msg

log :: Show val => Text -> val -> val
log msg val = unsafePerformIO $ do
  putLn $ "DEBUG: " <> msg <> " => " <> show val
  return val
