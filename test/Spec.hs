import RIO ( IO )

import Test.Hspec

import qualified Site.PublicResourcesSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PublicResourcesSpec"     Site.PublicResourcesSpec.spec
