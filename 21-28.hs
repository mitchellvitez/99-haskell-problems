import Test.Hspec
import System.Random
import System.Random.Shuffle
import Control.Monad

main = hspec $ do
  describe "problem 21" $ do
    it "inserts an element at a given position into a list" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

  describe "problem 22" $ do
    it "creates a list of all integers in a given range" $ do
      range 4 9 `shouldBe` [4..9]

  describe "problem 23" $ do
    it "extracts a given number of randomly selected elements from a list" $ do
      r <- rnd_select_IO "abcdefgh" 3
      length r `shouldBe` 3

  describe "problem 24" $ do
    it "draws n different random numbers from the set 1..m" $ do
      r <- diff_select 6 49
      length r `shouldBe` 6
      maximum r `shouldSatisfy` (<49)

  describe "problem 25" $ do
    it "generates random permutations" $ do
      r <- rnd_permu "abcdef"
      length r `shouldBe` 6

  -- TODO
  -- describe "problem 26" $ do
    -- it "generates combinations" $ do
      -- length (combinations 3 "abcdef") `shouldBe` 220

  -- TODO
  -- describe "problem 27" $ do

  -- describe "problem 28" $ do
  --   it "sorts a list of lists by length of sublist" $ do
  --     lsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe`
  --       ["o","de","de","mn","abc","fgh","ijkl"]

-- problem 21
insertAt c xs n = take (n-1) xs ++ [c] ++ drop (n-1) xs

-- problem 22
range a b = slice [1..] a b where
  slice xs a b = take (b-a+1) . drop (a-1) $ xs

-- problem 23
rnd_select :: [a] -> Int -> StdGen -> [a]
rnd_select xs n g =
  map (xs !!) . take n $ randomRs (0, length xs) g

rnd_select_IO :: [a] -> Int -> IO [a]
rnd_select_IO xs n = fmap (rnd_select xs n) getStdGen

-- problem 24
diff_select n m = do
  g <- getStdGen
  return . take n $ shuffle' [1..m] m g

-- problem 25
rnd_permu xs = do
  g <- getStdGen
  return $ shuffle' xs (length xs) g

-- problem 26
-- TODO
combinations n xs = undefined

-- problem 27
-- TODO

--problem 28
-- TODO
