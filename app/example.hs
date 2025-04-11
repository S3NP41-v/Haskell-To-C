module Main where

import Prelude

main :: IO ()
main = do
  let i = 100
  let ps = primes
  
  print (ps !! i)
  
  putStrLn "enter some string: "
  userInput <- getLine
  putStrLn userInput


primes :: [Prime]
primes = filter isPrime (m6xnp1 1)

isPrime :: Integer -> Bool
isPrime n = all (\x -> n `rem` x /= 0) [3,5.. floor (sqrt $ fromIntegral n)]

m6xnp1 :: Integer -> [Integer]
m6xnp1 n = n * 6 - 1 : n * 6 + 1 : m6xnp1 (n + 1)

type Prime = Integer

