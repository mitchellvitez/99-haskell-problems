import Test.Hspec
import Data.List (intercalate)

main = hspec $ do
  describe "problem 95" $ do
    it "writes numbers in full words as on checks" $ do
      fullWords 175 `shouldBe` "one-seven-five"

-- problem 95
fullWords = intercalate "-" . map toWord . digits
  where toWord = (!!) ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] 

digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

-- TODO
-- problem 96
-- problem 97
-- problem 98
-- problem 99
