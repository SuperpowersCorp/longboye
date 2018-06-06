{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LB20Spec ( spec ) where

import Longboye.Prelude hiding ( mod )

import Data.Text               ( unlines )
import LB20                    ( parseSource )
import LB20.Imports
import Test.Hspec
import Language.Haskell.Exts   ( ParseResult( ParseOk ) )

spec :: Spec
spec = describe "LB20" $ do

  context "adjustLocs" $ do

    it "should adjust decl locations properly (and nothing else)" $ do

      let ParseOk (mod, _)  = parseSource [] "test" exampleModText
          ParseOk (emod, _) = parseSource [] "test" expectedModText
      adjustLocs (3, 5) mod `shouldBe` emod

exampleModText :: Text
exampleModText = unlines
  [ "module Foo where"
  , "import Bar"
  , "foo :: Int"
  , "foo = 5"
  ]

expectedModText :: Text
expectedModText = unlines
  [ "module Foo where"
  , "import Bar"
  , ""
  , ""
  , ""
  , ""
  , ""
  , "foo :: Int"
  , "foo = 5"
  ]
