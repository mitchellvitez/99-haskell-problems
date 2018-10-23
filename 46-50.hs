import Test.Hspec
import Control.Monad

main = hspec $ do
  describe "problem 46" $ do
    it "builds a truth table for a binary logical expression" $ do
      table (\a b -> (and' a (or' a b))) `shouldBe`
        [ [True,True,True]
        , [True,False,True]
        , [False,True,False]
        , [False,False,False]
        ]

  describe "problem 47" $ do
    it "builds a truth table for a binary infix logical operator" $ do
      table (\a b -> a `and'` (a `or'` not b) ) `shouldBe`
        [ [True,True,True]
        , [True,False,True]
        , [False,True,False]
        , [False,False,False]
        ]

  describe "problem 48" $ do
    it "builds a truth table for an n-ary logical expression" $ do
      tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
        `shouldBe`
        [ [True,True,True,True]
        , [True,True,False,True]
        , [True,False,True,True]
        , [True,False,False,True]
        , [False,True,True,True]
        , [False,True,False,True]
        , [False,False,True,True]
        , [False,False,False,True]
        ]

  describe "problem 49" $ do
    it "constructs gray codes" $ do
      gray 3 `shouldBe` ["000","001","011","010","110","111","101","100"]

  -- TODO
  -- describe "problem 50" $ do
  --   it "constructs huffman codes from a frequency table" $ do
  --     huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)] `shouldBe`
  --       [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]


infixl 6 `and'`
and' True True = True
and' _ _ = False

infixl 4 `or'`
or' False False = False
or' _ _ = True

infixl 6 `nand'`
nand' True True = False
nand' _ _ = True

infixl 4 `nor'`
nor' False False = True
nor' _ _ = False

infixl 5 `xor'`
xor' a b | a == b = False
xor' _ _ = True

impl' False _ = True
impl' True True = True
impl' True False = False

infixl 3 `equ'`
equ' a b | a == b = True
equ' _ _ = False

-- problem 46, 47
table op = [[a, b, a `op` b] | a <- [True, False], b <- [True, False]]

-- problem 48
tablen n op = map (\args -> args ++ [op args]) $ replicateM n [True, False]

-- problem 49
gray 0 = [""]
gray n = map ('0':) g ++ map ('1':) (reverse g)
  where g = gray $ n - 1

-- problem 50
-- TODO
