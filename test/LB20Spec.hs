{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LB20Spec ( spec ) where

import Longboye.Prelude hiding ( mod )

import Data.Text               ( unlines )
import LB20                    ( parseSource )
import LB20.Imports
import Test.Hspec
import Language.Haskell.Exts   ( ParseResult( ParseOk )
                               , exactPrint
                               )

spec :: Spec
spec = describe "LB20" $ do

  context "adjustLocs" $ do

    it "should adjust decl locations properly (and nothing else)" $ do

      let ParseOk (mod, _) = parseSource [] "test" exampleMod
          expectedMod      = "module Foo where\nimport Bar\n\n\n\n\n\nfoo :: Int\nfoo = 5\n"
      exactPrint (adjustLocs (3, 5) mod) [] `shouldBe` expectedMod

exampleMod :: Text
exampleMod = unlines
  [ "module Foo where"
  , "import Bar"
  , "foo :: Int"
  , "foo = 5"
  ]
