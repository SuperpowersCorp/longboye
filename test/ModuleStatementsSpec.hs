{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ModuleStatementsSpec
       ( main
       , spec
       ) where

import Longboye.Prelude

import Data.String               ( unlines )
import Longboye.ModuleStatements ( interactS )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ModuleStatements.interact" $
  it "basics" $ do
    let sscce = unlines
          [ "{-# LANGUAGE ScopedTypeVariables #-}"
          , ""
          , "module Foo ( bar, baz ) where"
          , ""
          , "x = 5"
          ]
        extensions = []
        expected = unlines
          [ "{-# LANGUAGE ScopedTypeVariables #-}"
          , ""
          , "module Foo"
          , "     ( bar"
          , "     , baz"
          , "     ) where"
          , ""
          , "x = 5"
          ]
    interactS extensions sscce `shouldBe` expected
