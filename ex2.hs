{-
        TDA452: Functional Programming        
        Exercises

        Week 2: Recursion and Datatypes

        Gregor Ulm
-}

import Test.QuickCheck
import Data.Ix

-- 1. The Maximum Function

-- returns the maximum of two numbers
max' :: (Eq a, Ord a) => a -> a -> a
max' x y | x > y     = x
         | otherwise = y

prop_max1 :: Int -> Int -> Bool
prop_max1 p1 p2 | p1 > p2   = max' p1 p2 == p1
                | otherwise = max' p1 p2 == p2
        --where types = p1 :: Int

prop_max2 p q = max' p q >= p
                && max' p q >= q
                && max' p q `elem` [p,q]
        where types = p :: Int


-- 2. Sum of squares

-- computes the sum of squares from 1 to n
sumSquares :: Integer -> Integer
sumSquares n | n <= 0     = 0
             | otherwise  = n * n + sumSquares (n-1)

prop_sumSquares :: Integer -> Property
prop_sumSquares n = n >= 0 ==>
        sumSquares n == (n * (n + 1) * (2 * n + 1)) `div` 6


-- 3. Towers of Hanoi
{- 
   Note:
   This code was taken from:
   http://rosettacode.org/wiki/Towers_of_Hanoi#Haskell
-}

hanoi :: Integer -> a -> a -> a -> [(a, a)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

hanoiIO n = mapM_ f $ hanoi n 1 2 3 where
  f (x,y) = putStrLn $ "Move " ++ show x ++ " to " ++ show y


-- 4. Fibonacci

-- computes the nth Fibonacci number
fib :: Integer -> Integer
fib n | n == 0          = 1
      | n == 1          = 1
      | otherwise       = fib (n - 1) + fib (n - 2)

-- a much faster implementation
fib2 :: Integer -> Integer
fib2 n = fib2' n 1 1 

fib2' :: Integer -> Integer -> Integer -> Integer
fib2' i a b | i == 0     = a
            | otherwise  = fib2' (i - 1) b (a + b)  

prop_fib :: Integer -> Bool
prop_fib p = fib2 n == fib n
        where n = abs p


-- 5. Factors

-- finds the smallest factor of n
smallestFactor :: Integer -> Integer
smallestFactor n | n == 1       = 1
                 | otherwise    = nextFactor 2 n

-- helper function for smallest factor
nextFactor :: Integer -> Integer -> Integer
nextFactor k n | k == n         = n
               | n `mod` k == 0 = k 
               | otherwise      = nextFactor (k+1) n

prop_smallestFactor1 :: Integer -> Bool
prop_smallestFactor1 p = n `mod` smallestFactor n == 0
        where n = abs p

prop_smallestFactor2 :: Integer -> Bool
prop_smallestFactor2 p = (n `div` res) * res == n
        where n   = abs p
              res = smallestFactor  n 

prop_nextFactor :: Integer -> Property
prop_nextFactor p = n >= 2 ==> res > 1  && res <= n
        where n   = abs p
              res = nextFactor 2 n

-- computes the number of factors of n in the range 1..n
numFactors :: Integer -> Integer
numFactors n = numFactors' n n

numFactors' :: Integer -> Integer -> Integer
numFactors' n k | k == 1         = 1
                | n `mod` k == 0 = 1 + numFactors' n (k - 1)
                | otherwise      = numFactors' n (k - 1)


-- 6. Multiplying list elements

-- multiplies together all elements in a list
multiply :: (Num a) => [a] -> a
multiply []     = 1
multiply (x:xs) = x * multiply xs


-- 7. Avoiding duplicates

-- checks whether a list contains duplicate elements
duplicates :: Eq a => [a] -> Bool
duplicates []                   = False
duplicates (x:xs) | x `elem` xs = True
                  | otherwise   = duplicates xs

-- removes duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []                     = []
removeDuplicates (x:xs) | x `elem` xs   = removeDuplicates xs
                        | otherwise     = x : removeDuplicates xs

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))


-- 8. Testing

data Rank = Numeric Integer | Jack | Queen | King | Ace
        deriving (Show, Eq, Ord)

rankBeats :: Rank -> Rank -> Bool
rankBeats _     Ace               = False
rankBeats Ace   _                 = True
rankBeats _     King              = False
rankBeats King  _                 = True
rankBeats _     Queen             = False
rankBeats Queen _                 = False --True
rankBeats _     Jack              = False
rankBeats Jack  _                 = True
rankBeats (Numeric m) (Numeric n) = m > n

prop_rankBeats :: Rank -> Rank -> Bool
prop_rankBeats r1 r2 = r1 == r2 |+| r1 `rankBeats` r2 
                                |+| r2 `rankBeats` r1
-- defining XOR
infixr 2 |+|
True  |+| False = True
False |+| True  = True
_     |+| _     = False

instance Arbitrary Rank where
  arbitrary = frequency [ (1, return Jack)
                        , (1, return Queen)
                        , (1, return King)
                        , (1, return Ace)
                        , (9, do n <- choose (2, 10)
                                 return (Numeric n))
                        ]


-- 9. Defining Types

data Month = January | February | March | April | May | June
                | July | August | September | October | November
                | December
        deriving (Eq, Show)

-- determines days per month; note: simplified leap year assumption
daysInMonth :: Month -> Int -> Int
daysInMonth month year  | month == February && leapyear year   = 29
                        | month == February                    = 28
                        | even month                           = 30
                        | otherwise                            = 31
        where even month    = month `elem` [April, June, September, November]
              leapyear year = year `mod` 4 == 0



data Date = Date { year :: Int,
                   month :: Month,
                   day :: Int }
        deriving (Show)

-- exampleDate = Date 2000 January 29


-- checks whether given date is valid
validDate :: Date -> Bool
validDate d = year d >= 0
              && inRange(1, daysInMonth (month d) (year d)) (day d)  

-- e.g. validDate (Date (2000) January 19)
