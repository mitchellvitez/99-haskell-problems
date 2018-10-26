import Test.Hspec

data Tree a = Node a [Tree a]
  deriving (Show, Eq)

leaf x = Node x []

tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a'
  [ Node 'f' [Node 'g' []]
  , Node 'c' []
  , Node 'b' [Node 'd' [], Node 'e' []]
  ]

main = hspec $ do
  describe "problem 70C" $ do
    it "counts the nodes of a multiway tree" $ do
      nnodes tree2 `shouldBe` 2

-- problem 70C
nnodes (Node _ xs) = 1 + (sum $ map nnodes xs)

-- TODO
-- problem 70
-- problem 71
-- problem 72
-- problem 73
