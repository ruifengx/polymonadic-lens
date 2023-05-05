import Test.Hspec

main :: IO ()
main = hspec do
  describe "hspec" do
    specify "hspec works" do
      return () :: IO ()
