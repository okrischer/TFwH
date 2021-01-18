> module Numbers where

3.3 Computing Floors
====================

Definition of the function `until` from *standard prelude*, which gives us a basic looping mechanism:

```haskell
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)
```

We use this looping mechanism for a first (iterative) version of our `floor` function $\lfloor x \rfloor$, which returns the first integer lower than a given number:

> myFloor' :: Float -> Integer
> myFloor' x = if x < 0
>   then until ((<=x) . fromInteger) (subtract 1) (-1)
>   else until ((>x) . fromInteger) (+1) 1 - 1

This may seem awkward, but there is in deed no direct conversion from an `Integral` number to a floating-point number in haskell.
We could use the `properFraction` function from `RealFrac` type class, as the definition of `floor` in *standard prelude* does, but here the conversion is done in a similar manner, although more efficiently.

```haskell
floor x = if r < 0 then n-1 else n
    where (n,r) = properFraction x
```

Therefore, let's give our own attempt a shot.

Binary Search
-------------

A better method for computing floor is to first find integers *m* and *n* such that $m\leq x < n$ and then shrink the interval $(m,n)$ to a unit interval (one with $m + 1 = n$) that contains x.

> myFloor :: Float -> Integer
> myFloor x = fst (until unit (shrink x) (bound x))
>   where unit (m,n) = (m+1 == n)

> type Interval = (Integer, Integer)
> shrink :: Float -> Interval -> Interval
> shrink x (m,n) = if ((fromInteger p) <= x) then (p,n) else (m,p)
>   where p = choose (m,n)

> choose :: Interval -> Integer
> choose (m,n) = (m+n) `div` 2

> bound :: Float -> Interval
> bound x = (lower x, upper x)

> lower :: Float -> Integer
> lower x = until ((<=x) . fromInteger) (*2) (-1)

> upper :: Float -> Integer
> upper x = until ((>x) . fromInteger) (*2) 1

3.4 Natural Numbers
===================

