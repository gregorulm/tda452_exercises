{-
        TDA452: Functional Programming
        Exercises

        Week 5: Recursive Datatypes (Part A)

        Gregor Ulm
-}

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Test.QuickCheck

-- 0. Expression and Integer Trees

-- Expression Tree

{-
data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

showExpr :: Expr -> String
showExpr (Lit n)     = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++ ")"


-- A. Evaluations

{-

eval (Num 67)
-- error!

eval (Add (Sub (Lit 3) (Lit 1)) (Lit 3))
eval (Sub (Lit 3) (Lit 1))      + eval (Lit 3)
(eval (Lit 3) - eval (Lit 1))   + 3
(3 - 1)                         + 3
2                               + 3
5

showExpr (Add (Lit 67) (Lit (-34)))
"(" ++ showExpr (Lit 67) ++ "+" ++ showExpr (Lit (-34)) ++ ")"
"(" ++ show 67 ++ "+" ++ show (-34) ++ ")"
"(67+-34)"

-}


-- B. Counting operators

size :: Expr -> Int
size (Lit n)    = 0
size (Add e1 e2)  = 1 + size e1 + size e2
size (Sub e1 e2)  = 1 + size e1 + size e2

-}


-- C. Adding Operations

{-
data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

{-                
eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2
-}

-- eval redefined to account for division by zero
eval :: Expr -> Maybe Int
eval (Lit n)     = Just n
eval (Add e1 e2) = liftM2 (+) (eval e1) (eval e2)
eval (Sub e1 e2) = liftM2 (-) (eval e1) (eval e2)
eval (Mul e1 e2) = liftM2 (*) (eval e1) (eval e2)
eval (Div e1 e2) | fromJust check == 0 = Nothing
                 | otherwise           = liftM2 div (eval e1) check
                        where check = (eval e2) 

showExpr :: Expr -> String
showExpr (Lit n)     = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ "*" ++ showExpr e2 ++ ")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ "/" ++ showExpr e2 ++ ")"

size :: Expr -> Int
size (Lit n)    = 0
size (Add e1 e2)  = 1 + size e1 + size e2
size (Sub e1 e2)  = 1 + size e1 + size e2
size (Mul e1 e2)  = 1 + size e1 + size e2
size (Div e1 e2)  = 1 + size e1 + size e2
-}


-- D. Using operator definitions
{-
data Expr = Lit Int
          | Op Ops Expr Expr

data Ops = Add | Sub | Mul | Div | Mod

eval :: Expr -> Maybe Int
eval (Lit n)        = Just n
eval (Op Add e1 e2) = liftM2 (+) (eval e1) (eval e2)
eval (Op Sub e1 e2) = liftM2 (-) (eval e1) (eval e2)
eval (Op Mul e1 e2) = liftM2 (*) (eval e1) (eval e2)
eval (Op Div e1 e2) = if fromJust check == 0
                      then Nothing
                      else liftM2 div (eval e1) check
                      where check = (eval e2)
eval (Op Mod e1 e2) = if fromJust check == 0
                      then Nothing
                      else liftM2 mod (eval e1) check
                      where check = (eval e2)

showExpr :: Expr -> String
showExpr (Lit n)     = show n
showExpr (Op Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (Op Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++ ")"
showExpr (Op Mul e1 e2) = "(" ++ showExpr e1 ++ "*" ++ showExpr e2 ++ ")"
showExpr (Op Div e1 e2) = "(" ++ showExpr e1 ++ "/" ++ showExpr e2 ++ ")"
showExpr (Op Mod e1 e2) = "(" ++ showExpr e1 ++ "/" ++ showExpr e2 ++ ")"

size :: Expr -> Int
size (Lit n)    = 0
size (Op Add e1 e2)  = 1 + size e1 + size e2
size (Op Sub e1 e2)  = 1 + size e1 + size e2
size (Op Mul e1 e2)  = 1 + size e1 + size e2
size (Op Div e1 e2)  = 1 + size e1 + size e2
size (Op Mod e1 e2)  = 1 + size e1 + size e2
-}

-- E. Defining infix operators

data Expr = Lit Int
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr ://: Expr -- modulus

eval :: Expr -> Maybe Int
eval (Lit n)        = Just n
eval (e1 :+: e2)  = liftM2 (+) (eval e1) (eval e2)
eval (e1 :-: e2)  = liftM2 (-) (eval e1) (eval e2)
eval (e1 :*: e2)  = liftM2 (-) (eval e1) (eval e2)
eval (e1 :/: e2) = if fromJust check == 0
                   then Nothing
                   else liftM2 div (eval e1) check
                   where check = eval e2
eval (e1 ://: e2) = if fromJust check == 0
                    then Nothing
                    else liftM2 mod (eval e1) check
                    where check = eval e2

showExpr :: Expr -> String
showExpr (Lit n)     = show n
showExpr (e1 :+: e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (e1 :-: e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++ ")"
showExpr (e1 :*: e2) = "(" ++ showExpr e1 ++ "*" ++ showExpr e2 ++ ")"
showExpr (e1 :/: e2) = "(" ++ showExpr e1 ++ "/" ++ showExpr e2 ++ ")"
showExpr (e1 ://: e2) = "(" ++ showExpr e1 ++ "mod" ++ showExpr e2 ++ ")"

size :: Expr -> Int
size (Lit n)    = 0
size (e1 :+: e2)  = 1 + size e1 + size e2
size (e1 :-: e2)  = 1 + size e1 + size e2
size (e1 :*: e2)  = 1 + size e1 + size e2
size (e1 :/: e2)  = 1 + size e1 + size e2
size (e1 ://: e2)  = 1 + size e1 + size e2



-- Integer Tree

data NTree = NilT
           | Node Int NTree NTree
        deriving (Show, Eq)
           
sumTree :: NTree -> Int
sumTree NilT            = 0
sumTree (Node n t1 t2)  = n + sumTree t1 + sumTree t2

depth :: NTree -> Int
depth NilT              = 0
depth (Node n t1 t2)    = 1 + max (depth t1) (depth t2)


-- A. Calculations
{-
sumTree (Node 3 (Node 4 NilT NilT) NilT)
3 + sumTree (Node 4 NilT NilT) + sumTree NilT
3 + (4 + sumTree NilT + sumTree NilT) + 0
C
7

depth (Node 3 (Node 4 NilT NilT) NilT)
1 + max (depth (Node 4 NilT NilT))          (depth NilT)
1 + max (1 + max (depth NilT) (depth NilT)) 0
1 + max (1 + max 0            0           ) 0
1 + max (1 + 0                            ) 0
1 + max  1                                  0
1 +      1
2     
-}


-- B. Return sub-trees

data Sub = L | R

rightSubTree :: NTree -> NTree
rightSubTree = subTree R

leftSubTree :: NTree -> NTree
leftSubTree = subTree L

subTree :: Sub -> NTree -> NTree
subTree _ NilT           = NilT
subTree L (Node n t1 t2) = t1
subTree R (Node n t1 t2) = t2


-- C. Determine whether a given number is an element of a given NTree

inTree :: Int -> NTree -> Bool
inTree _ NilT                           = False
inTree n (Node x t1 t2) | n == x        = True
                        | otherwise     = inTree n t1 || inTree n t2


-- D. Maximum & Minimum

maxTree :: NTree -> Int
maxTree NilT             = error "tree contains no elements"
maxTree (Node int t1 t2) = foldTree max int (Node int t1 t2)

minTree :: NTree -> Int
minTree NilT             = error "tree contains no elements"
minTree (Node int t1 t2) = foldTree min int (Node int t1 t2)

foldTree :: (Int -> Int -> Int) -> Int -> NTree -> Int
foldTree f z NilT           = z;
foldTree f z (Node x t1 t2) = x `f` (foldTree f z t1 `f` foldTree f z t2)

-- minTree (Node 6 NilT (Node 9 (Node 19 (Node 67 NilT NilT) (Node 33 NilT NilT)) NilT))


-- E. Reflect a Tree, i.e. swap left and right sub-trees

reflectTree :: NTree -> NTree
reflectTree NilT                = NilT
reflectTree (Node int t1 t2)    = Node int t2 t1

prop_reflect :: NTree -> Bool
prop_reflect t = reflectTree (reflectTree t) == t

{-
-- generator for arbitrary (balanced) NTree
sizedArbNTree :: Int -> Gen NTree
sizedArbNTree 0 = return NilT
sizedArbNTree n = do
                int <- arbitrary
                t1 <- (sizedArbNTree (n-1))
                t2 <- (sizedArbNTree (n-1))
                return $ (Node int t1 t2)
-}

-- generator for arbitrary NTree, with depth n
sizedArbNTree :: Int -> Gen NTree
sizedArbNTree 0 = return NilT
sizedArbNTree n = do
                int <- arbitrary
{-
                t1 <- frequency [(2, (sizedArbNTree 0)),
                                 (3, (sizedArbNTree (n-1)))]
                t2 <- frequency [(2, (sizedArbNTree 0)),
                                 (3, (sizedArbNTree (n-1)))]
-}
                t1 <- oneof [sizedArbNTree 0, sizedArbNTree (n-1)]
                t2 <- oneof [sizedArbNTree 0, sizedArbNTree (n-1)]
                return (Node int t1 t2)

-- sample $ sizedArbNTree 4
                
instance Arbitrary NTree where
        arbitrary = sized sizedArbNTree


-- F. Collapsing an NTree

-- turns an Ntree into a list
collapse :: NTree -> [Int]
collapse NilT           = []
collapse (Node x t1 t2) = [x] ++ collapse t1 ++ collapse t2

-- collapse (Node 3638 (Node (-1578) (Node 3648 NilT NilT) (Node (-1257) NilT (Node (-3972) (Node 3143 NilT NilT) NilT))) (Node 3247 NilT NilT))


-- collapses an NTree into a sorted list
sortTree :: NTree -> [Int]
sortTree t = sortTree' $ collapse t

sortTree' :: [Int] -> [Int]
sortTree' []        = []
sortTree' [x]       = [x]
sortTree' (x:xs)    = sortTree' loEq ++ [x] ++ sortTree' hi
                        where loEq = [ y | y <- xs, y <= x ]
                              hi   = [ y | y <- xs, y > x ]
                                                       
-- property: the sum of an NTree is the same as the sum of a collapsed NTree
prop_sum :: NTree -> Bool
prop_sum t = sumTree t == sum (collapse t)

-- property: a collapsed and sorted NTree is sorted
prop_sorted :: NTree -> Bool
prop_sorted t = isSorted $ sortTree t

-- checks whether a list is sorted
isSorted []             = True
isSorted [x]            = True
isSorted (x:y:ys)       = x <= y && isSorted (y:ys)


-- 1. File Systems

-- A. Data definition

data File = Data String
          | Directory [File] String
        deriving (Eq, Show)

fs0 = Data "file1"

fs1 = Directory [Data "file1", Data "file2"] "dir1"

fs2 = Directory [Data "file1",
                 Directory [] "dir2"] "dir1"

fs3 = Directory [Data "file1",
                 Directory [Data "file1", Data "file2"] "dir2"] "dir1"

fs4 = Directory [Directory [Data "file1", Data "file2"] "dir2"] "dir1"

fs5 = Directory [Data "file1",
                 Directory [Data "file2", Data "file3"] "dir2",
                 Directory [Data "file4", Data "file5"] "dir3"] "dir1" 

fs6 = Directory [Data "file1",
                 Directory [Data "file2", Data "file3"] "dir2",
                 Directory [Data "file4", 
                             Directory [Data "file5", Data "file6" ] "dir4"] "dir3"] "dir1" 


-- B. Searching for a file

-- returns all paths in which file x was found
searchFile :: String -> File -> [String]
searchFile x fs = filter (not . isSuffixOf "//") searchResult
        where searchResult = searchFile' x fs "/"

-- searchFile "file1" fs3
-- ["/dir1/file1","/dir1/dir2/file1"]
 

searchFile' :: String -> File -> String -> [String]
searchFile' needle (Data name) _ | needle == name = [needle]
                                 | otherwise      = [name ++ "//"]
                -- "/" can't be part of a file name, therefore used to indicate that
                -- the file was not found

searchFile' needle (Directory fs name) path =
        [ path ++ name ++ "/" ++ concat (searchFile' needle f "") | f <- files ] 
        ++ concat [ searchFile' needle d (path ++ name ++ "/") | d <- directories ]
        where files                  = filter isData fs
              directories            = filter (not . isData) fs
              isData (Data _)        = True
              isData (Directory _ _) = False 



-- 2. Exercises on Propositional Logic

-- A. Representing propositions

data Pr = Var String
        | And Pr Pr
        | Or Pr Pr
        | Not Pr
        deriving (Eq, Show)

prop1 = And (Var "A") (Var "B")              
prop2 = Or (Var "A") (And (Var "C") (Var "A") )  
prop3 = Or (Var "P") (Not (Var "P"))
prop4 = And (Var "p") (Var "q")           

vals1 = [("p",True),("q",False)]


-- B. Determining truth values

-- returns list of the variables in a proposition
vars :: Pr -> [String]
vars p = nub $ vars' p

vars' :: Pr -> [String]
vars' (Var x)   = [x]
vars' (And x y) = vars' x ++ vars' y
vars' (Or x y)  = vars' x ++ vars' y
vars' (Not p)   = vars' p


-- determines whether a proposition is true
truthValue :: Pr -> [(String, Bool)] -> Bool
truthValue prop values = truthValue' prop
        where truthValue' (Var x)   = fromJust $ lookup x values
              truthValue' (And x y) = truthValue' x && truthValue' y 
              truthValue' (Or x y)  = truthValue' x || truthValue' y
              truthValue' (Not x)   = not $ truthValue' x

-- truthValue (Var "p") vals1


-- C. Tautology checker

tautology :: Pr -> Bool
tautology prop = and [ truthValue prop (zip vs bs) | bs <- bss ]
        where vs  = vars prop
              bss = bools $ length vs 

bools :: Int -> [[Bool]]
bools 0  = [[]]
bools n  = map (False:) bss ++ map (True:) bss
                where bss = bools (n-1)
