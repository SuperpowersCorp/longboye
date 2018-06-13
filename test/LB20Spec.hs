{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LB20Spec ( spec ) where

import Longboye.Prelude      hiding ( SrcLoc
                                    , mod
                                    )

import Control.Lens
import Data.Data.Lens               ( biplate )
import Data.Text                    ( unlines
                                    , unpack
                                    )
import LB20                         ( parseSource )
import LB20.Imports
import LB20.Lenses
import Language.Haskell.Exts
import Test.Hspec

spec :: Spec
spec = describe "LB20" $ do
  adjustLocsSpec
  formatSpec

adjustLocsSpec :: Spec
adjustLocsSpec = describe "adjustLocs" $ do
  let ParseOk (mod, _)  = parseSource [] "test" exampleModText

  context "lens for adjustLocs" $ do

    it "should be able to view the SrcSpanInfo's" $ do
      let l = biplate :: Traversal' (Module SrcSpanInfo) SrcSpanInfo
      toListOf l mod `shouldBe`
        [ SrcSpanInfo { srcInfoSpan = SrcSpan "test" 1 1 5 1
                      , srcInfoPoints = [ SrcSpan "test" 1 1 1 1
                                        , SrcSpan "test" 1 1 1 1
                                        , SrcSpan "test" 2 1 2 1
                                        , SrcSpan "test" 3 1 3 1
                                        , SrcSpan "test" 4 1 4 1
                                        , SrcSpan "test" 5 1 5 1
                                        , SrcSpan "test" 5 1 5 1]}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 1 1 1 17
                      , srcInfoPoints = [ SrcSpan "test" 1 1 1 7
                                        , SrcSpan "test" 1 12 1 17]}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 1 8 1 11
                      , srcInfoPoints = []}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 2 1 2 11
                      , srcInfoPoints = [ SrcSpan "test" 2 1 2 7]}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 2 8 2 11
                      , srcInfoPoints = []}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 3 1 3 11
                      , srcInfoPoints = [ SrcSpan "test" 3 5 3 7]}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 3 1 3 4
                      , srcInfoPoints = []}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 3 8 3 11
                      , srcInfoPoints = []}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 3 8 3 11
                      , srcInfoPoints = []}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 3 8 3 11
                      , srcInfoPoints = []}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 4 1 4 8
                      , srcInfoPoints = []}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 4 1 4 4
                      , srcInfoPoints = []}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 4 1 4 4
                      , srcInfoPoints = []}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 4 5 4 8
                      , srcInfoPoints = [ SrcSpan "test" 4 5 4 6]}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 4 7 4 8
                      , srcInfoPoints = []}
        , SrcSpanInfo { srcInfoSpan = SrcSpan "test" 4 7 4 8
                      , srcInfoPoints = []}]

    it "should be able to get al the way to the srcSpans" $ do
      let l = (biplate :: Traversal' (Module SrcSpanInfo) SrcSpanInfo) . srcSpanL
      toListOf l mod `shouldBe`
        [ SrcSpan "test" 1 1 5 1
        , SrcSpan "test" 1 1 1 17
        , SrcSpan "test" 1 8 1 11
        , SrcSpan "test" 2 1 2 11
        , SrcSpan "test" 2 8 2 11
        , SrcSpan "test" 3 1 3 11
        , SrcSpan "test" 3 1 3 4
        , SrcSpan "test" 3 8 3 11
        , SrcSpan "test" 3 8 3 11
        , SrcSpan "test" 3 8 3 11
        , SrcSpan "test" 4 1 4 8
        , SrcSpan "test" 4 1 4 4
        , SrcSpan "test" 4 1 4 4
        , SrcSpan "test" 4 5 4 8
        , SrcSpan "test" 4 7 4 8
        , SrcSpan "test" 4 7 4 8
        ]

    it "should be able to get all the way to the srcSpanStartLine" $ do
      let l = (biplate :: Traversal' (Module SrcSpanInfo) SrcSpanInfo)
            . srcSpanL . srcSpanStartLineL
      toListOf l mod `shouldBe` [1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4]

    it "should be able to set the srcSpanStartLine on a srcSpan" $ do
      let srcLoc1 = SrcLoc "filename" 1 1
          srcLoc2 = SrcLoc "filename" 1 2

          srcSpan :: SrcSpan
          srcSpan = mkSrcSpan srcLoc1 srcLoc2
      srcSpan `shouldBe` SrcSpan "filename" 1 1 1 2
      (srcSpan & srcSpanStartLineL .~ 5) `shouldBe` SrcSpan "filename" 5 1 1 2

    it "should be able to set the srcSpanEndLine on a srcSpan" $ do
      let srcLoc1 = SrcLoc "filename" 1 1
          srcLoc2 = SrcLoc "filename" 1 2

          srcSpan :: SrcSpan
          srcSpan = mkSrcSpan srcLoc1 srcLoc2
      srcSpan `shouldBe` SrcSpan "filename" 1 1 1 2
      (srcSpan & srcSpanEndLineL .~ 5) `shouldBe` SrcSpan "filename" 1 1 5 2

    it "should be able to increment the numbers" $ do
      let l = ((biplate :: Traversal' (Module SrcSpanInfo) SrcSpanInfo)
                . srcSpanL . srcSpanStartLineL)
          l' = ((biplate :: Traversal' (Module SrcSpanInfo) SrcSpanInfo)
                . srcSpanL . srcSpanStartLineL) :: ASetter' (Module SrcSpanInfo) Int
          mod' = mod & l' .~ 1
      toListOf l mod' `shouldBe` [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

  context "adjustLocs itself" $ do

    it "should adjust decl locations properly (and nothing else)" $ do
      let ParseOk (_emod, _) = parseSource [] "test" expectedModText
      exactPrint (adjustLocs (3, 5) mod) [] `shouldBe` "module Foo where\nimport Bar\n\n\n\n\n\nfoo :: Int\nfoo = 5"
      -- do we need the srcinfo points?
--      adjustLocs (3, 5) mod `shouldBe` emod

formatSpec :: Spec
formatSpec = describe "format" $ do

  context "when called on a module with no imports" $ do

    -- it "should return the module unchanged" $ property $ \mod -> do
    --   let mod' :: Module SrcSpanInfo = removeImports mod
    --   format mod' `shouldBe` mod'
    --
    -- removeImports :: Module a -> Module a
    -- removeImports = undefined

    it "should return the module unchanged" $ do
      let ParseOk (mod, _)  = parseSource [] "test" noImportsModText
      exactPrint (format mod) [] `shouldBe` unpack noImportsModText

  context "when called on a module with one import" $ do

    it "should return the module unchanged" $ do
      let ParseOk (mod, _)  = parseSource [] "test" oneImportModText
      exactPrint (format mod) [] `shouldBe` unpack oneImportModText

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

noImportsModText :: Text
noImportsModText = unlines
  [ "module Foo where"
  , ""
  , "foo :: Int"
  , "foo = 5"
  ]

oneImportModText :: Text
oneImportModText = unlines
  [ "module Foo where"
  , ""
  , "import Foo (Bar)"
  , ""
  , "foo :: Int"
  , "foo = 5"
  ]
