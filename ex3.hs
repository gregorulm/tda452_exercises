{-
        TDA452: Functional Programming        
        Exercises

        Week 3: Lists and List Comprehensions

        Gregor Ulm
-}


import Data.List
import Data.Ix
import Test.QuickCheck


-- 0. Defining functions over lists

-- A. Redefining Prelude functions

-- sample definition of "take"
take' :: Int -> [a] -> [a]
take' 0 _              = []
take' _ []             = []
take' n (x:xs) | n > 0 = x : take' (n-1) xs
take' _ _              = error "PreludeList.take: negative argument"

-- two definitions of "drop", the first being a bit clumsy
drop' :: (Eq a) => Int -> [a] -> [a]
drop' 0 xs              = xs
drop' _ []              = []
drop' n (x:xs) | n > 0  = drop' (n-1) xs
               | n <= 0 = x:xs

-- ...and a more elegant version:
drop'' :: (Eq a) => Int -> [a] -> [a]
drop'' n xs | n <= 0    = xs
            | null xs   = []                 
drop'' n (x:xs)         = drop'' (n-1) xs

-- show equivalence of all definitions of "drop"
prop_drop :: Int -> [Bool] -> Bool
prop_drop p q = drop' p' q == drop'' p' q
                && drop'' p' q == drop p' q
        where p' = p `mod` (length q + 1) -- do avoid division by zero error


-- define splitAt
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs | n <= 0          = ([], xs)
              | n >= length xs  = (xs, [])
              | otherwise       = (take n xs, drop n xs)

-- showing equivalence of "splitAt" and "splitAt'"
prop_splitAt :: Int -> [Bool] -> Bool
prop_splitAt n xs = splitAt n' xs == splitAt' n' xs
        where n' = n `mod` (length xs + 1)


-- B. Two definitions of "zip3"

-- recursive definition
zip3' :: (Eq a) => [a] -> [a] -> [a] -> [(a,a,a)]
zip3' xs ys zs | [] `elem` [xs, ys, zs]        = []
zip3' (x:xs) (y:ys) (z:zs)                     = (x, y, z) : zip3' xs ys zs

-- using "zip"
zip3'' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3'' xs ys zs = reorder $ zip xs $ zip ys zs

reorder :: [(a,(b,c))] -> [(a,b,c)]
reorder []             = []
reorder ((a,(b,c)):xs) = (a,b,c) : reorder xs

-- verifying equivalence
prop_zip3 :: [Int] -> [Int] -> [Int] -> Bool
prop_zip3 p q r = zip3 p q r == zip3' p q r
                  && zip3'' p q r == zip3' p q r



-- 1. Permutations

-- determines whether one list is a permutation of another
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []     = True
isPermutation [] _      = False
isPermutation (x:xs) ys | x `elem` ys = isPermutation xs (ys \\ [x])
                        | otherwise   = False 

prop_permutation :: [Int] -> Bool
prop_permutation xs = isPermutation (reverse xs) xs 


-- 2. Avoiding Duplicates

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


-- 3. Pascal's Triangle

-- returns the nth row of Pascal's triangle
pascal :: Int -> [Int]
pascal n = pascal' n 1 [1]

pascal' n m lst    | n == m = lst
                   | m < n  = pascal' n (m+1) ([1] ++ addAdjacent lst ++ [1])

-- computes sum of adjacent numbers of previous row
addAdjacent (x:[])   = []
addAdjacent (x:y:xs) = (x + y) : addAdjacent(y:xs)


-- 4. Erastosthenes' Sieve

-- removes all multiples of m from ns
crossOut :: Int -> [Int] -> [Int]
crossOut m ns = [ n  | n <- ns, n `mod` m /= 0 ]

-- applies Eratosthenes' sieve to given list of numbers
sieve :: [Int] -> [Int]
sieve []        = []
sieve (n:ns)    = n : sieve (crossOut n ns)


-- 5. Number Games

-- tests whether n is prime in the range [2..100]
isPrime :: Int -> Bool
isPrime n       = n `elem` sieve [2..100]

-- tests whether n is the sum of primes in the range [2..100]
sumOfPrimes :: Int -> Bool
sumOfPrimes n = any isPrime allDifferences
        where allDifferences = [ n - x | x <- sieve [2..100], x < n ]

-- two tests:
prop_isPrime = all isPrime (sieve [2..100])
prop_sumOfPrimes = and [ sumOfPrimes n | n <- [4..100], n `mod` 2 == 0 ]


-- 6. Occurrences in Lists

-- checks whether n is in xs
occursIn :: Eq a => a -> [a] -> Bool
occursIn n xs = [ x |  x <- xs, n == x ] /= []

-- checks whether all xs are in ys
allOccurIn :: Eq a => [a] -> [a] -> Bool
allOccurIn xs ys = and [ occursIn x ys | x <- xs ]   

-- checks whether ys and xs have exactly the same elements
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = allOccurIn xs ys && allOccurIn ys xs

-- computes how often x occurs in xs
numOccurrences :: Eq a => a -> [a] -> Int
numOccurrences x xs = sum [ if n == x then 1 else 0 | n <- xs ]  

-- coverts a list into a bag
bag :: Eq a => [a] -> [(a, Int)]
bag xs = nub [ (x, numOccurrences x xs) | x <- xs ]


-- 7. Elements and Positions

-- converts a list into a list of pairs of elements and their positions
positions :: (Eq a) => [a] -> [(a, [Int])]
positions xs = positions' $ zip xs [1..length xs]

positions' :: (Eq a) => [(a,Int)] -> [(a,[Int])]
positions' [] = []
positions' ((a,b):xs) = (a, b:all_pos) : positions' xs'
        where occurrences = filter ((== a) . fst) xs
              xs' = xs \\ occurrences
              all_pos = [ pos | (a, pos) <- occurrences ]


-- returns the first position at which x occurs in xs
firstPosition :: (Eq a) => a -> [a] -> Int
firstPosition x xs | x `notElem` xs  = 0
                   | otherwise       = firstPosition' x xs
                where firstPosition' x (y:xs)   | x == y    = 1
                                                | otherwise = 1 + firstPosition' x xs

-- removes the first occurrence of x from xs
remove1 :: (Eq a) => a -> [a] -> [a]
remove1 x xs = remove1' (firstPosition x xs) xs

remove1' 0 xs    = xs
remove1' 1 (y:ys) = ys
remove1' n (y:ys) = y : remove1' (n-1) ys


-- removes the first n occurrences of x from xs
remove :: (Eq a) => Int -> a -> [a] -> [a]
remove _ _ []                   = []
remove 0 x xs                   = xs
remove n x (y:ys) | x == y      = remove (n-1) x ys
                  | otherwise   = y : remove n x ys


-- 8. More List Comprehensions

-- finds all Pythagorean triples with a <= b <= c <= 100
pythagoreanTriples :: [(Int,Int,Int)]
pythagoreanTriples = [ (a, b, c) | a <- [1..100],
                                    b <- [1..100],
                                    c <- [1..100],
                                    a ^ 2 + b ^ 2 == c ^ 2 ]
