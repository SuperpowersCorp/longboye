{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ModuleStatementsSpec
       ( main
       , spec
       ) where

import qualified Prelude
import           Longboye.Prelude

import           Data.String                    ( unlines )
import           Longboye.Files                 ( mkInteractor )
import           Longboye.ModuleStatementParser ( parseE )
import           Longboye.ModuleStatements      ( cleanText )
import           Test.Hspec

main :: IO ()
main = hspec spec

interactS :: Prelude.String -> Prelude.String
interactS = mkInteractor parseE cleanText extensions
  where
    extensions = []

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
    interactS sscce `shouldBe` expected
