module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Array (null)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

-- 4.1 (Easy) Write a recursive function which returns true if and only if its
-- input is an even integer.
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

-- 4.2 (Medium) Write a recursive function which counts the number 
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


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

