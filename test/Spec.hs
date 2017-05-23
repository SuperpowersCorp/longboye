

import Data.Monoid      ( (<>) )
import Longboye.Imports ( interactS )
import Test.Hspec

-- import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Imports.interact" $ do
    it "handles (:<|>)(..) correctly" $ do
      let sscce = "import Foo ( (:<|>)(..) )"
      (interactS sscce) `shouldBe` ("\n\n" <> sscce <> "\n\n")
