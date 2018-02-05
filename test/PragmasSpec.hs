{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module PragmasSpec
       ( main
       , spec
       ) where

import Longboye.Prelude

import Data.String      ( unlines )
import Longboye.Pragmas ( interactS )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Pragmas.interact" $ do
    it "basics" $ do
      let sscce = unlines
            [ "{-# LANGUAGE NoImplicitPrelude #-}"
            , "{- # OPTIONS_GHC -fno-warn-foo #-}"
            , "{- # LANGUAGE ScopedTypeVariables, LambdaCase #-}"
            , "  {-#  LANGUAGE  FlexibleInstances    #-} "
            , ""
            , "module Foo where"
            , "x = 5"
            ]
          extensions = []
          expected = unlines
            [ "{-# LANGUAGE NoImplicitPrelude   #-}"
            , "{-# LANGUAGE FlexibleInstances   #-} "
            , "{-# LANGUAGE LambdaCase          #-} "
            , "{-# LANGUAGE ScopedTypeVariables #-} "
            , "{- # OPTIONS_GHC -fno-warn-foo   #-}"
            , ""
            , "module Foo where"
            , "x = 5"
            ]
      interactS extensions sscce `shouldBe` expected
