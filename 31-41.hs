import Test.Hspec
import Data.List (foldl', group)

main = hspec $ do
  describe "problem 31" $ do
    it "determines whether a given integer is prime" $ do
      isPrime 2 `shouldBe` True
      isPrime 7 `shouldBe` True
      isPrime 27 `shouldBe` False

  describe "problem 32" $ do
    it "determines the greatest common divisor of two positive integers" $ do
      [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] `shouldBe` [9,3,3]

  describe "problem 33" $ do
    it "determines whether two positive integers are coprime" $ do
      coprime 35 64 `shouldBe` True

  describe "problem 34" $ do
    it "calculates euler's totient function" $ do
      totient 10 `shouldBe` 4

  describe "problem 35" $ do
    it "lists the prime factors of a positive integer in ascending order" $ do
      primeFactors 315 `shouldBe` [3, 3, 5, 7]

  describe "problem 36" $ do
    it "lists the prime factors of a positive integer and their multiplicites" $ do
      primeFactorsMult 315 `shouldBe` [(3,2),(5,1),(7,1)]

  describe "problem 37" $ do
    it "calculates euler's totient function (improved)" $ do
      phi 10 `shouldBe` 4

  describe "problem 39" $ do
    it "constructs a list of all primes in a range" $ do
      primesR 10 20 `shouldBe` [11,13,17,19]

  describe "problem 40" $ do
    it "finds the two prime numbers that sum up to a given even integer" $ do
      goldbach 28 `shouldBe` (5, 23)

  describe "problem 41" $ do
    it "finds goldbach composition of all even numbers in a range" $ do
      goldbachList 9 20 `shouldBe` [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
      goldbachList' 4 2000 50 `shouldBe` [(73,919),(61,1321),(67,1789),(61,1867)]

-- problem 31
isPrime x | x > 1 = null $ filter ((==0) . rem x) [2..x-1]
isPrime _ = False

-- problem 32
myGCD a b
  | b == 0 = abs a
  | otherwise = myGCD b (a `mod` b)

-- problem 33
coprime a b = (==1) $ gcd a b

-- problem 34
totient n = length [x | x <- [1..n], coprime x n]

-- problem 35
primeFactors 1 = []
primeFactors n = 
  k : primeFactors (n `div` k)
  where k = head [x | x <- [2..n], isPrime x, n `rem` x == 0]

-- problem 36
primeFactorsMult = map toMult . group . primeFactors
  where toMult xs = (head xs, length xs)

-- problem 37
phi = product . map transform . primeFactorsMult
  where transform (p, m) = (p - 1) * p ^ (m - 1)

-- problem 39
primesR a b = filter isPrime [a..b]

-- problem 40
goldbach n = head [(a, b) | a <- ps, b <- ps, a + b == n]
  where ps = primesR 1 n

-- problem 41
goldbachList a b = map goldbach $ filter even [a..b]

goldbachList' a b lim = filter (\x -> fst x > lim) $ goldbachList a b
