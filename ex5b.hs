{-
        TDA452: Functional Programming
        Exercises

        Week 5: Recursive Datatypes (Part B)

        Gregor Ulm
-}

import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random
import Data.List
import Data.Maybe

-- 3. Exercises on type Expr

-- given definitions

data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Var Name
          deriving (Eq)

type Name = String

vars :: Expr -> [Name]
vars (Var v)   = [v] 
vars (Add e f) = vars e `union` vars   f
vars (Mul e f) = vars e `union` vars   f
vars _         = []

ex1 = Mul (Add (Num 1) (Num 2)) (Num 4) 
ex2 = Add (Num 1) (Mul (Num 2) (Num 4))
ex3 = Num 1 `Add` (Num 2 `Mul` Num 4)

eval :: [(Name,Integer)] -> Expr -> Integer
eval _   (Num m)   = m
eval env (Add e f) = eval env e + eval env f
eval env (Mul e f) = eval env e * eval env f
eval env (Var x)   = fromJust (lookup x env)


instance Show Expr where
 show = showExpr

showExpr :: Expr -> String
showExpr (Var x)   = x
showExpr (Num n)   = show n
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor e         = showExpr e


instance Arbitrary Expr where
  arbitrary = sized arbExpr

arbExpr :: Int -> Gen Expr
arbExpr s =
  frequency [ (1,genNum), (1, genVar)
            , (s,genOp Add) 
            , (s,genOp Mul)
            ]
  where genNum = do n <- arbitrary
                    return (Num n)
        genOp op = do a <- arbExpr s'
                      b <- arbExpr s'
                      return (op a b)
        s' = s `div` 2

genVar = elements [Var "x", Var "y", Var "z"]



-- A. Differentiation

diff :: Expr -> Name -> Expr
diff (Num n)   x = Num 0
diff (Add a b) x = Add (diff a x) (diff b x)
diff (Mul a b) x = Add (Mul a (diff b x)) (Mul b (diff a x))
diff (Var y)   x | x == y       = Num 1
                 | otherwise    = Num 0

-- property: derivative contains <= variables than initial expression
prop_diff1 e = forAll (elements ["x", "y", "z"])
                $ \ arb e -> length (vars (diff e arb)) <= length (vars e)

-- property: initial expression contains >= variables than its derivative
prop_diff2 e = forAll (elements ["x", "y", "z"])
                $ \ arb e -> length (vars e) >= length (vars (diff e arb))


-- B. Simplifying an expression

{-

 Note: the following definitions only work for linear equations, like the examples
       given in the exercise sheet:

        2+3 --> 5
        2*x+6+5*x+6 --> 7*x+12
        0*x+-2+5*y+3 --> 5*y+1
        
        However, they won't work for equations of a higher degree.
-}


-- the Integer is the constant term
type Polynomial = ([Element], Integer)

-- coefficient, variable name
type Element = (Integer,Name)

-- sample expressions
e1 = Add (Num 2) (Num 3)
e2 = Add (Add (Add
                   (Mul (Num 2) (Var "x")) (Num 6))
                   (Mul (Num 5) (Var "x")))
                   (Num 6)
e3 = Add (Add (Add 
               (Mul (Num 0) (Var "x")) (Num (-2)))
               (Mul (Num 5) (Var "y")))
               (Num 3)
  

-- simplifies an expression
simplify :: Expr -> Expr
simplify e = buildExpr (removeZero $ combine el) c 
        where (c, el) = convert e


-- converts an expression into a format that allows easier manipulation
convert :: Expr -> (Integer, [(Name,Integer)])
convert e = convert' e 0 []

convert' :: Expr -> Integer -> [(Name,Integer)] -> (Integer, [(Name,Integer)])
convert' (Add e (Num a))               i expr = convert' e (i + a) expr
convert' (Add e (Var x))               i expr = convert' e i       ((x,1):expr)
convert' (Add e (Mul (Num a) (Var x))) i expr = convert' e i       ((x,a):expr)
convert' (Add e (Mul (Var x) (Num a))) i expr = convert' e i       ((x,a):expr)
convert' (Mul (Num a) (Var x))         i expr = (i,           (x,a):expr)
convert' (Mul (Num a) (Num b))         i expr = ((a * b) + i, expr)
convert' (Num a)                       i expr = (i + a,       expr)
convert' (Var x)                       i expr = (i,           (x,1):expr)


-- adding up values of variables
combine :: [(Name,Integer)] -> [(Name,Integer)]
combine lst = combine' lst []

combine' :: [(Name,Integer)] -> [(Name,Integer)] -> [(Name,Integer)]
combine' [] acc = acc
combine' ((var, x):xs) acc | isNothing $ lookup var acc = combine' xs ((var,x):acc)
                           | otherwise = combine' xs newAcc
                                where oldVal = fromJust $ lookup var acc
                                      newVal = oldVal + x
                                      newAcc = insert (var, newVal) (delete (var, oldVal) acc)

-- remove variables that occur zero times
removeZero :: [(Name,Integer)] -> [(Name,Integer)]
removeZero [] = []
removeZero ((var, x):xs) | x == 0    = removeZero xs
                         | otherwise = (var,x): removeZero xs


buildExpr :: [(Name,Integer)] -> Integer -> Expr
buildExpr [] i = Num i
buildExpr xs 0 = buildExpr' xs
buildExpr xs i = Add (buildExpr' xs) (Num i)

buildExpr' :: [(Name,Integer)] -> Expr
buildExpr' ((var, x):[]) = Mul (Num x) (Var var) 
buildExpr' ((var, x):xs) = Add (Mul (Num x) (Var var)) (buildExpr' xs)

-- createExpr e1
-- createExpr e2
-- createExpr e3

prop_SimplifyCorrect e =
    eval env e == eval env (simplify e)
        where env = [("x", 10), ("y", 5), ("z", 4)]
