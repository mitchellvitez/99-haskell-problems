import Test.Hspec

main = hspec $ do
  describe "problem 1" $ do
    it "gives the last element of a list" $ do
      myLast [1..4] `shouldBe` 4
      myLast "xyz" `shouldBe` 'z'

  describe "problem 2" $ do
    it "gives the second-last element of a list" $ do
      myButLast [1..4] `shouldBe` 3
      myButLast ['a'..'z'] `shouldBe` 'y'

  describe "problem 3" $ do
    it "finds the kth element of a list" $ do
      elementAt [1,2,3] 2 `shouldBe` 2
      elementAt "haskell" 5 `shouldBe` 'e'

  describe "problem 4" $ do
    it "finds the number of elements of a list" $ do
      myLength [123,456,789] `shouldBe` 3
      myLength "Hello, world!" `shouldBe` 13

  describe "problem 5" $ do
    it "reverses a list" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe`
        "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1..9] `shouldBe` [9,8..1]

  describe "problem 6" $ do
    it "checks whether a list is a palindrome" $ do
      isPalindrome [1,2,3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True

  describe "problem 7" $ do
    it "flattens a nested list structure" $ do
      flatten (Elem 5) `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
        `shouldBe` [1..5]

  describe "problem 8" $ do
    it "eliminates consecutive duplicates of list elements" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

  describe "problem 9" $ do
    it "packs consecutive duplicates into sublists" $ do
      pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

  describe "problem 9" $ do
    it "performs run-length encoding" $ do
      encode "aaaabccaadeeee" `shouldBe`
        [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]


-- problem 1
myLast [] = error "no last element of empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

-- problem 2
myButLast [x,_] = x
myButLast (x:xs) = myButLast xs
myButLast _ = error "no second-last element"

-- problem 3
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- problem 4
myLength = sum . map (\x -> 1)

-- problem 5
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- problem 6
isPalindrome xs = xs == reverse xs

-- problem 7
data NestedList a = Elem a | List [NestedList a]

flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- problem 8
compress [] = []
compress (x:y:ys) | x == y = compress (y:ys)
compress (x:xs) = x : compress xs

-- problem 9
pack [] = []
pack l@(x:xs) = takeWhile (==x) l : pack (dropWhile (==x) xs)

-- problem 10
encode = map (\x -> (length x, head x)) . pack 
