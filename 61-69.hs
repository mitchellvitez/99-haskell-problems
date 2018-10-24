import Test.Hspec

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

leaf x = Branch x Empty Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

main = hspec $ do
  describe "problem 61" $ do
    it "counts the leaves of a binary tree" $ do
      countLeaves tree4 `shouldBe` 2

  describe "problem 61A" $ do
    it "finds the leaves of a binary tree" $ do
      leaves tree4 `shouldBe` [4,2]

  describe "problem 62" $ do
    it "finds the internal nodes of a binary tree" $ do
      internals tree4 `shouldBe` [1,2]

  describe "problem 62" $ do
    it "finds the nodes at a given level of a binary tree" $ do
      atLevel tree4 2 `shouldBe` [2,2]

-- problem 61
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- problem 61A
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

-- problem 62
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x : internals l ++ internals r

-- problem 62B
atLevel Empty _ = []
atLevel (Branch x l r) n
  | n == 1 = [x]
  | n > 1 = atLevel l (n-1) ++ atLevel r (n-1)
  | otherwise = []
