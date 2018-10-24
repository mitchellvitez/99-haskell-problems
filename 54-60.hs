import Test.Hspec

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

leaf x = Branch x Empty Empty

main = hspec $ do
  -- TODO
  -- describe "problem 55" $ do
  --   it "constructs balanced binary trees" $ do
  --     cbalTree 4 `shouldBe` 
  --       [ Branch 'x' (Branch 'x' Empty Empty) 
  --            (Branch 'x' Empty 
  --                        (Branch 'x' Empty Empty))
  --       , Branch 'x' (Branch 'x' Empty Empty) 
  --            (Branch 'x' (Branch 'x' Empty Empty) 
  --                        Empty)
  --       , Branch 'x' (Branch 'x' Empty 
  --                        (Branch 'x' Empty Empty)) 
  --            (Branch 'x' Empty Empty)
  --       , Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
  --                        Empty) 
  --            (Branch 'x' Empty Empty)
  --       ]

  describe "problem 56" $ do
    it "checks whether a binary tree is symmetric" $ do
      symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` False
      symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) `shouldBe` True

  describe "problem 57" $ do
    it "constructs a binary search tree from a list of numbers" $ do
      construct [3, 2, 5, 7, 1] `shouldBe`
        Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
      symmetric (construct [5, 3, 18, 1, 4, 12, 21]) `shouldBe` True
      symmetric (construct [3, 2, 5, 7, 4]) `shouldBe` False

-- TODO
-- problem 55
-- cbalTree 0 = [Empty]
-- cbalTree 1 = [leaf 'x']
-- cbalTree n = undefined

-- problem 56
mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ c d) = mirror a c && mirror b d
mirror _ _ = False

symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

-- problem 57
add x Empty = leaf x
add x (Branch y l r)
  | x < y = Branch y (add x l) r
  | x > y = Branch y l (add x r)
  | otherwise = Branch y l r

construct xs = foldl (flip add) Empty xs

-- TODO
-- problem 58
-- problem 59
-- problem 60
