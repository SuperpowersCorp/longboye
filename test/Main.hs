module Main
       ( main
       , spec
       ) where

import           Data.Monoid                         ( (<>) )
import           Data.Text                           ( isInfixOf )
import           Longboye.Import                     ( Import )
import qualified Longboye.Import           as Import
import           Longboye.Import.Arbitrary           ()
import           Longboye.Imports                    ( interactS )
import           Test.Hspec
import           Test.QuickCheck                     ( property )
import qualified Test.QuickCheck           as QC
import           Test.QuickCheck.Random              ( mkQCGen )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Imports.interact" $
    it "handles (:<|>)(..) correctly" $ do
      let sscce      = "import Foo ( (:<|>)(..) )"
          extensions = []
      interactS extensions sscce `shouldBe` ("\n\n" <> sscce <> "\n\n")

  describe "Imports.format" $ do
    it "Never stacks closing parens" $ property $ prop_neverStacksParensAcrossLines

    it "sorts members" $ do
      let imports      = "import Foo ( foo, bar, bif, baz )"
          extensions   = []
          expected     = unlines [ ""
                                 , ""
                                 , "import Foo ( bar"
                                 , "           , baz"
                                 , "           , bif"
                                 , "           , foo"
                                 , "           )"
                                 , ""
                                 ]
      interactS extensions imports `shouldBe` expected


    it "sorts sub-members" $ do
      let imports      = "import Foo ( foo, Bar(c,b,d,a), bif, baz )"
          extensions   = []
          expected     = unlines [ ""
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
      interactS extensions imports `shouldBe` expected

prop_neverStacksParensAcrossLines :: Bool -> Bool -> Int -> Int -> Import -> Bool
prop_neverStacksParensAcrossLines anyQ anyH maxModLen maxAsLen imp =
  -- TODO: this should actually fail because of single line imports but it's not
  not $ ") )" `isInfixOf` (Import.format anyQ anyH maxModLen maxAsLen imp)

_tmpTest :: IO ()
_tmpTest =
  QC.quickCheckWith args prop_neverStacksParensAcrossLines
  where args = QC.stdArgs{QC.replay = Just (mkQCGen seed, 0)}
        seed = 99666056
