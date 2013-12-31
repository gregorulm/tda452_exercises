{-
        TDA452: Functional Programming        
        Exercises

        Week 4: IO, Test Data and Properties

        Gregor Ulm
-}


import Data.List
import Data.Maybe
import Test.QuickCheck
import System.Random
import System.Directory
import System.IO
import System.Process
import Control.Monad

-- 0. Basic IO
-- A.

-- sums up n integers
readInt :: IO ()
readInt =
        do 
        putStrLn "How many numbers do you want to sum up?"
        n <- getLine
        let count = read n :: Int
        loop count 0 0
        where loop :: Int -> Int -> Int -> IO ()
              loop n count sum | n == count  = putStrLn $ "The total is " ++ show sum
                               | otherwise   = do
                                      putStrLn "Enter number:"
                                      x <- getLine
                                      let val = read x :: Int
                                      loop n (count + 1) (sum + val)


-- B.

-- reads integers until 0 is entered, outputs sorted list
readSequence :: IO [Int]
readSequence = loop []
        where loop acc =  do
                putStrLn "Enter a number:"
                x <- getLine
                let value = read x :: Int
                if value == 0
                then return $ sort acc
                else loop (value : acc)


-- C.
-- repeats op until the condition test is True
repeat' :: IO Bool -> IO () -> IO ()
repeat' test op = do
                x <- test
                unless x (do op
                             repeat' test op)                
{-
repeat'' test op = do
                      x <- test
                      if x
                      then return ()
                      else do
                              op
                              repeat' test op
-}

-- repeat' (return True) (putStrLn "foo")
-- repeat' (return False) (putStrLn "foo")
-- repeat' (getLine >>= (\x -> return (x == "stop"))) (putStrLn "foo")
-- repeat' (fmap (== "stop") getLine) (putStrLn "foo")
-- repeat' ((== "stop") <$> getLine) (putStrLn "foo")


-- 1. Properties of the Look function

-- looks up an element in a list
look :: Eq a => a -> [(a,b)] -> Maybe b
look x []                       = Nothing
look x ((x',y):xys) | x == x'   = Just y
                    | otherwise = look x xys

prop_LookNothing p xs = look p xs == Nothing ==> not $ p `elem` [ a | (a,b) <- xs ]
        where types = xs::[(Int, Int)]

prop_LookJust p xs = isJust (look p xs) ==> fromJust (look p xs) `elem` [ b | (a,b) <- xs ]
        where types = xs::[(Bool, Int)]

prop_Look p xs = if isNothing (look p xs)
                 then p `notElem` [ a | (a,b) <- xs ]
                 else fromJust (look p xs) `elem` [ b | (a,b) <- xs ]
                 where types = xs::[(Bool, Int)]


--2. Monadic helper functions

-- takes a list of instructions, creates one big instruction that executes
-- entire list of instructions
sequence' :: Monad m => [m a] -> m [a] 
sequence' = foldr k (return [])
            where k m m' = do { x <- m; xs <- m'; return (x:xs) }

sequence'' :: [IO a] -> IO [a]
sequence'' []     = return []
sequence'' (i:is) = do
                    a  <- i
                    --as <- is
                    as <- sequence'' is
                    return $ a:as
-- sequence'' [return (2 > 4), return (2 == 2)]
-- sequence'' [return 1, return 2, return 3, return 4]
-- sequence'' $ map return [1..4]
                         

-- creates an instruction that only executes the argument instruction if the boolean is True
onlyIf :: Monad m => Bool -> m () -> m ()
onlyIf = when

{-
    Here are three alternative definitions, the first without the eta reduction,
    the second with a redundant return statement, the third with a more verbose
    if-then-else:

onlyIf' b f = when b f

onlyIf'' b f = when b (do x <- f
                         return x)

onlyIf''' b f = if b
               then do x <- f
                       return x
               else return ()
-}

-- onlyIf False (putStrLn "foo")
-- onlyIf True (putStrLn "foo")



-- 3. Number guessing game

game :: IO ()
game = do
       putStrLn "Think of a number between 1 and 100!"
       loop 0 100
       where loop low high = do
                putStrLn $ "Is it " ++ show avg ++ "?"
                val <- getLine
                case val of
                        "yes"    -> putStrLn "Great, I won!"
                        "higher" -> loop avg high
                        "lower"  -> loop low avg  
                where avg = (low + high) `div` 2        


-- 4. Backup Script

{-
        Use "system" to write a program that
        - creates a new directory called "backup", using the "mkdir" command,
        - copies all the files in the current directory into the backup, using
          cp or copy as appropriate
-}

backupFiles :: IO ()
backupFiles = do
        x <- system "mkdir backup"
        y <- readProcess "ls" [] []
        let fileList = tail $ words y
        z <- sequence [ system $ "cp " ++ x ++ " backup" | x <- fileList ]
        putStrLn "done"


-- alternative definition w/o using "system"
backupFiles' :: IO ()
backupFiles' = do
        _ <- createDirectory "backup"
        files <- readProcess "ls" [] []
        let fileList = tail $ words files
        _ <- sequence [ copyFile x ("backup/" ++ x) | x <- fileList]
        putStrLn "done"

        
        
-- 5. Generating Lists

-- A.

-- a simple generator
oneInt :: Gen Integer
oneInt = arbitrary

{-
oneInt' = do
        x <- arbitrary   --choose(1,10)
        return x
-}

listOf' :: Integer -> Gen a -> Gen [a]
listOf' n g = sequence [ g | _ <- [1..n] ]
-- sample $ listOf' 10 oneInt


-- property: generated lists are of given length
prop_gen :: NonNegative Integer -> Property
prop_gen (NonNegative n) = forAll (listOf' n arb)
                                $ \xs -> length xs == fromIntegral n
          where arb = arbitrary :: Gen Int


-- B. 

-- generates pairs of lists of the same random length
pairsOfLists :: Gen a -> Gen ([a], [a])
pairsOfLists g = 
        do
        n <- arbitrary
        x <- listOf' (abs n) g
        y <- listOf' (abs n) g
        return (x, y)


-- C.

prop_unzipzip = forAll (pairsOfLists arb)
                        $ \xs -> unzip (uncurry zip xs) == xs
        where arb = arbitrary :: Gen Char

prop_zipunzip = forAll (pairsOfLists arb)
                        $ \xs -> uncurry zip (unzip (uncurry zip xs)) == uncurry zip xs
        where arb = arbitrary :: Gen Char



-- 6. Generating Ordered Lists

-- A.

-- checks whether a list is ordered
ordered :: (Ord a, Eq a) => [a] -> Bool
ordered []                      = True
ordered (x:[])                  = True
ordered (x:y:ys) | x <= y       = ordered (y:ys)
                 | otherwise    = False 


ordered' :: (Ord a, Eq a) => [a] -> Bool
ordered' (x:y:ys) = x <= y && ordered (y:ys)


-- B.

-- generator for random positive integers
onePosInt :: Gen Integer
onePosInt = do
        x <- arbitrary
        return $ abs x

-- generates list of length n with positive integers,
-- interpreted as the list of differences, used in "orderedList'"
differences :: Integer -> Gen [Integer]
differences n = listOf' n onePosInt
-- sample $ differences 4

-- generator for random ordered lists of integers
orderedList' :: Gen [Integer]
orderedList' = do n     <- arbitrary
                  lst   <- differences $ abs n
                  start <- arbitrary                   
                  makeList (abs start) lst
               where makeList start lst = return $ start : makeList' start lst
                     makeList' a []     = []
                     makeList' a (x:xs) = (a + x) : makeList' (a + x) xs

-- property: ordered list is indeed ordered
prop_orderedLists :: Property
prop_orderedLists = forAll orderedList' ordered
