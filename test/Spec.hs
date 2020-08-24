import qualified Spec.Catalogue
import qualified Spec.Chord
import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec Spec.Catalogue.spec
  hspec Spec.Chord.spec
