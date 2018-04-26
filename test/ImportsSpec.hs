{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ImportsSpec
       ( main
       , spec
       ) where

import qualified Prelude
import           Longboye.Prelude

import           Data.Text                           ( isInfixOf
                                                     , lines
                                                     )
import           Longboye.Files                      ( mkInteractor )
import           Longboye.Import                     ( Import )
import qualified Longboye.Import           as Import
import           Longboye.Import.Arbitrary           ()
import           Longboye.Imports                    ( cleanText )
import           Longboye.ImportsParser              ( parseE )
import           Test.Hspec
import           Test.QuickCheck                     ( property )
import qualified Test.QuickCheck           as QC
import           Test.QuickCheck.Random              ( mkQCGen )

main :: IO ()
main = hspec spec

interactS :: Prelude.String -> Prelude.String
interactS = mkInteractor parseE cleanText extensions
  where
    extensions = []

spec :: Spec
spec = do
  describe "Imports.interact" $
    it "handles (:<|>)(..) correctly" $ do
      let sscce      = "import Foo ( (:<|>)(..) )"
      interactS sscce `shouldBe` ("\n\n" <> sscce <> "\n\n")

  describe "Imports.format" $ do

    it "Never stacks closing parens" $ property $
      prop_neverStacksParensAcrossLines

    it "sorts members" $ do
      let imports      = "import Foo ( foo, bar, bif, baz )"
          expected     = Prelude.unlines
            [ ""
            , ""
            , "import Foo ( bar"
            , "           , baz"
            , "           , bif"
            , "           , foo"
            , "           )"
            , ""
            ]
      interactS imports `shouldBe` expected

    it "sorts sub-members" $ do
      let imports      = "import Foo ( foo, Bar(c,b,d,a), bif, baz )"
          expected     = Prelude.unlines
            [ ""
            , ""
            , "import Foo ( Bar( a"
            , "                , b"
            , "                , c"
            , "                , d"
            , "                )"
            , "           , baz"
            , "           , bif"
            , "           , foo"
            , "           )"
            , ""
            ]
      interactS imports `shouldBe` expected

prop_neverStacksParensAcrossLines :: Bool -> Bool -> Int -> Int -> Import -> Bool
prop_neverStacksParensAcrossLines anyQ anyH maxModLen maxAsLen imp =
  singleLine || not stacked
  where
    singleLine = (== 1) . length . lines $ formatted
    stacked    = ") )" `isInfixOf` formatted
    formatted  = Import.format anyQ anyH maxModLen maxAsLen imp

_tmpTest :: IO ()
_tmpTest =
  QC.quickCheckWith args prop_neverStacksParensAcrossLines
  where
    args = QC.stdArgs{QC.replay = Just (mkQCGen seed, 0)}
    seed = 99666056
