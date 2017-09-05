module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Array (null, filter, length, (..))
import Data.Array.Partial (head, tail)
import Data.Foldable (product)
import Data.Ring
import Partial.Unsafe (unsafePartial)

import Control.MonadZero (guard)

-- 4.4.1 (Easy) Write a recursive function which returns true if and only if its
-- input is an even integer.
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

-- 4.4.2 (Medium) Write a recursive function which counts the number 
-- of even integers in an array. Hint: the function unsafePartial head 
-- (where head is also imported from Data.Array.Partial) can be used 
-- to find the first element in a non-empty array.
countEvens :: Array Int -> Int
countEvens arr =
  if null arr 
    then 0
    else toInteger (isEven (unsafePartial head arr)) + countEvens (unsafePartial tail arr)
    where 
    toInteger :: Boolean -> Int
    toInteger false = 0
    toInteger true = 1

-- 4.7.1
-- (Easy) Use the map or <$> function to write a function which calculates 
-- the squares of an array of numbers.
square :: Array Number -> Array Number
square arr =
  map (\n -> n * n) arr

-- 4.7.2
-- (Easy) Use the filter function to write a function which removes the 
-- negative numbers from an array of numbers.
removeNegatives :: Array Number -> Array Number
removeNegatives arr =
  filter (\n -> n >= 0.0) arr

-- 4.7.3
-- (Medium) Define an infix synonym <$?> for filter. Rewrite your answer to 
-- the previous question to use your new operator. Experiment with the 
-- precedence level and associativity of your operator in PSCi.
infix 8 filter as <$?>

removeNegatives' :: Array Number -> Array Number
removeNegatives' arr =
  (\n -> n >= 0.0) <$?> arr

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

-- 4.11.1
-- (Easy) Use the factors function to define a function isPrime which 
-- tests if its integer argument is prime or not.
factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n =
  length (factors n) == 1