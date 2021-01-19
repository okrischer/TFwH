module Chap4
where

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x:iterate' f (f x)

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f = head . filter p . iterate' f
