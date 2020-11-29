module Types where

-- lazy evaluation
-- the function infinity runs, if called directly, until infinity
infinity = 1 + infinity

-- three is a non-strict function, which returns 3, regardless of the argument given to it
three x = 3

-- if we now call three with infinity as it's argument, infinity is not evaluated, 
-- because of Haskells lazy evaluation; instead, three just returns 3, since the 
-- evaluation of it's argument isn't necessary for returning the result

-- three infinity

-- creating data structures and mapping to type classes
type Pin        = Int
type FirstName  = String
type LastName   = String
type Adress     = [String]

{-
data Person = Person Pin FirstName LastName Adress
    deriving (Show, Eq)

samePerson :: Person -> Person -> Bool
samePerson p1 p2 = (pin p1 == pin p2)

pin :: Person -> Pin
pin person = this_pin
    where (Person this_pin _ _ _) = person
-}

-- the same with record syntax, i.e. giving every field an name,
-- from which haskell generates accessor functions
data Person = Person {
    personPin       :: Pin,
    personFirst     :: FirstName,
    personLast      :: LastName,
    personAdress    :: Adress
} deriving (Show, Eq)

samePerson :: Person -> Person -> Bool
samePerson p1 p2 = personPin p1 == personPin p2

-- using parameterized types for creating generic data types
data Maybe a = Data a
             | Null
    deriving (Show)

-- using parameterized types for creating recursive types
data List a = Cons a (List a)
            | Nil
    deriving (Show)

-- building a binary tree
data Tree a = Node a (Tree a) (Tree a)
            | Empty
    deriving (Show)

simpleTree = Node "root" (Node "left" Empty Empty)
                         (Node "right" Empty Empty)

-- catching errors with Maybe
-- using error terminates the calculation:
second :: [a] -> a
second xs   | null (tail xs) = error "list contains only one element"
            | otherwise = head (tail xs)

-- using Maybe returns an empty value in case of exception
safeSecond :: [a] -> Types.Maybe a
safeSecond []   = Null
safeSecond xs   | null (tail xs) = Null
                | otherwise = Data (head (tail xs))

-- even nicer with pattern matching
tidySecond :: [a] -> Types.Maybe a
tidySecond (_:x:_)  = Data x
tidySecond _ = Null
