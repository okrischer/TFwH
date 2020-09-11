module Exercises where
import Pipe
import qualified Data.Char as Char
import qualified Data.List as List

main :: IO()
main = do
  print (modernise "The morphology of prex - an essay in meta-algorithmics")
  putStr "lazy evaluation of susan: "
  print (susan' (== 't') "test")
  putStr "exp' 2 10 = "
  print (exp' 2 10)
  putStr "exp'' 2 10 = "
  print (exp'' 2 10)

modernise :: String -> String
modernise old = words old $> map capitalise $> unwords

capitalise :: String -> String
capitalise (char:chars) = Char.toUpper char : chars

--modernise "The morphology of prex - an essay in meta-algorithmics"

--eager implementation of susan
susan :: (a -> Bool) -> [a] -> a
susan p =  head . filter p

--susan (== 't') "test"
--susan (== 't') "Tesla" --throws error: empty list

--lazy implementation of susan
susan' :: (a -> Bool) -> [a] -> Maybe a
susan' = List.find

--susan' (== 't') "test"
--susan' (== 't') "Tesla"

--lazy implementation if beaver 
beaver :: (a -> Bool) -> [a] -> Maybe a
beaver p xs | null xs = Nothing
             | p x = Just x
             | otherwise = beaver p (tail xs)
             where x = head xs

--beaver (== 't') "test"
--beaver (== 't') "Tesla"

exp' :: Int -> Int -> Int
exp' x n | n == 0 = 1
         | n == 1 = x
         | otherwise = x * exp' x (n-1)
        
--exp' 2 10

exp'' :: Int -> Int -> Int
exp'' x n | n == 0 = 1
          | n == 1 = x
          | even n = exp'' (x*x) m
          | odd n  = x * exp'' (x*x) m
        where m = n `div` 2
        
--exp'' 2 10


