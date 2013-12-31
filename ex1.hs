{-
        TDA452: Functional Programming        
        Exercises

        Week 1: Getting Started with Haskell

        Gregor Ulm
-}

-- 1. Simple examples

-- SEK to GBP Converter
pounds :: Double -> Double
pounds kr = kr / 12.7775

-- Celsius to Fahrenheit (and vice versa)
celsiusToFahrenheit :: Double -> Double
celsiusToFahrenheit c = 32 + (c * 9/5)

-- Fahrenheit to Celsius
fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius f = (f - 32) * 5/9


-- 2. Comparison and more complex functions

-- Price based on quantitiy v
price :: Double -> Double
price v | v <= 10   = 3.5 * v
        | v <= 20   = 5 + 3*v
        | otherwise = 15 + 2.5*v

-- Average of two values
average :: Double -> Double -> Double
average x y = (x + y) / 2

-- Average of three values
average3 :: Double -> Double -> Double -> Double
average3 x y z = (x + y + z) / 3

-- Simplified leap year calculator
daysInYear :: Int -> Int
daysInYear x | x `mod` 4 == 0 = 366
               | otherwise      = 355


-- 3. Hailstone sequence

-- generates next value
next :: Int -> Int
next x  | x `mod` 2 == 0 = x `div` 2
        | otherwise      = x * 3 + 1

-- counts numbers of steps
steps :: Int -> Int
steps x | x == 1    = 1
        | otherwise = 1 + steps (next x)

-- produces sequence, starting from a given number
numbers :: Int -> [Int]
numbers n | n == 1    = [1]
          | otherwise = n : numbers (next n)
