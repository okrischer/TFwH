{- 
replace haskell operators for function composition and function application 
with pipe-like operators `.>` and `$>`, which evaluate in usual order from left to right
-}
module Pipe ( (.>), ($>) ) where

-- function composition
compose :: (a -> b) -> (b -> c) -> (a -> c) 
compose f g = \ x -> g (f x)

infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = compose f g

-- function application
apply :: a -> (a -> b) -> b
apply x f = f x

infixl 0 $>
($>) :: a -> (a -> b) -> b
x $> f = apply x f 
