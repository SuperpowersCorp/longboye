module Debug
       ( crash
       , log
       ) where

import           Prelude                  hiding ( log )
import           Overture

import           Data.Monoid                     ( (<>) )
import           Data.Text                       ( Text )
import qualified Data.Text        as Text
import           System.IO.Unsafe                ( unsafePerformIO )

crash :: Text -> a
crash msg = error . Text.unpack $ "Debug.crash: " <> msg

log :: Show val => Text -> val -> val
log msg val = unsafePerformIO $ do
  putLine ("DEBUG: " <> msg <> " => " <> show_ val)
  return val
