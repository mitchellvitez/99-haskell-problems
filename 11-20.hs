import Test.Hspec

main = hspec $ do
  describe "problem 11" $ do
    it "performs modified run length encoding" $ do
      encodeModified "aaaabccaadeeee" `shouldBe`
        [Multiple 4 'a',Single 'b',Multiple 2 'c',
         Multiple 2 'a',Single 'd',Multiple 4 'e']

  describe "problem 12" $ do
    it "decodes a run-length encoded list" $ do
      decodeModified 
        [Multiple 4 'a',Single 'b',Multiple 2 'c',
         Multiple 2 'a',Single 'd',Multiple 4 'e']
         `shouldBe` "aaaabccaadeeee"

  describe "problem 13" $ do
    it "encodes a run-length list directly" $ do
      encodeDirect "aaaabccaadeeee" `shouldBe`
        [Multiple 4 'a',Single 'b',Multiple 2 'c',
         Multiple 2 'a',Single 'd',Multiple 4 'e']

  describe "problem 14" $ do
    it "duplicates the elements of a list" $ do
      dupli [1,2,3] `shouldBe` [1,1,2,2,3,3]

  describe "problem 15" $ do
    it "replicates the elements of a list" $ do
      repli "abc" 3 `shouldBe` "aaabbbccc"

  describe "problem 16" $ do
    it "drops every nth element from a list" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

  describe "problem 17" $ do
    it "splits a list into two parts given the length of the first part" $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

  describe "problem 18" $ do
    it "extracts a slice from a list" $ do
      slice "abcdefghik" 3 7 `shouldBe` "cdefg"

  describe "problem 19" $ do
    it "rotates a list to the left" $ do
      rotate ['a'..'h'] 3 `shouldBe` "defghabc"
      rotate ['a'..'h'] (-2) `shouldBe` "ghabcdef"

  describe "problem 20" $ do
    it "removes the kth element from a list" $ do
      removeAt 2 "abcd" `shouldBe` ('b',"acd")

-- problem 11
data ListItem a = Multiple Int a | Single a
  deriving (Show, Eq)

pack [] = []
pack l@(x:xs) = takeWhile (==x) l : pack (dropWhile (==x) xs)

encodeModified = map toListItem . pack where
  toListItem x
    | length x == 1 = Single (head x)
    | otherwise = Multiple (length x) (head x)

-- problem 12
decodeModified [] = []
decodeModified ((Multiple n x):rest) = replicate n x ++ decodeModified rest
decodeModified ((Single x):rest) = x : decodeModified rest

-- problem 13
encodeDirect [] = []
encodeDirect (x:xs) = 
  toListItem len x : encodeDirect (dropWhile (==x) xs) where 
    len = length $ takeWhile (==x) (x:xs)
    toListItem len x = if len == 1 then Single x else Multiple len x

-- problem 14
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- problem 15
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

-- problem 16
dropEvery xs n = 
  snd . unzip . filter (keep . fst) $ zip [1..] xs
  where keep x = x `mod` n /= 0

-- problem 17
split xs n =
  (take' n xs, drop' n xs) where
    take' _ [] = []
    take' 0 _ = []
    take' n (x:xs) = x : take' (n-1) xs
    drop' _ [] = []
    drop' 0 xs = xs
    drop' n (x:xs) = drop' (n-1) xs

-- problem 18
slice xs a b = take (b-a+1) . drop (a-1) $ xs

-- problem 19
rotate xs n = suffix ++ prefix where
  (prefix, suffix) = split xs idx
  idx = if n < 0 then length xs + n else n 

-- problem 20
removeAt n xs = (char, rest) where
  char = head $ drop (n-1) xs
  rest = take (n-1) xs ++ drop n xs
