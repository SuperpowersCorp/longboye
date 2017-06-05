module Main ( main, spec ) where

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

  describe "Imports.format" $
    it "Never stacks closing parens" $ property $ prop_neverStacksParensAcrossLines


prop_neverStacksParensAcrossLines :: Bool -> Bool -> Int -> Int -> Import -> Bool
prop_neverStacksParensAcrossLines anyQ anyH maxModLen maxAsLen imp =
  not $ ") )" `isInfixOf` (Import.format anyQ anyH maxModLen maxAsLen imp)

_tmpTest :: IO ()
_tmpTest =
  QC.quickCheckWith args prop_neverStacksParensAcrossLines
  where args = QC.stdArgs{QC.replay = Just (mkQCGen seed, 0)}
        seed = 99666056
