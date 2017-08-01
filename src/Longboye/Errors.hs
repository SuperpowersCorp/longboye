module Longboye.Errors ( renderError ) where

import           Data.Text                            ( Text )
import qualified Data.Text                    as Text
import           Language.Haskell.Exts.SrcLoc         ( SrcLoc )

renderError :: SrcLoc -> String -> Text
renderError srcLoc err = Text.pack $ "ERROR at " ++ show srcLoc ++ ": " ++ err
