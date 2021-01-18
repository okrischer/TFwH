import Criterion.Main

expNaiv :: Integer -> Integer -> Integer
expNaiv x n 
  | n == 0 = 1
  | n == 1 = x
  | otherwise = x * expNaiv x (n-1)

expNaiv2 = expNaiv 2 

expOpt :: Integer -> Integer -> Integer
expOpt x n 
  | n == 0 = 1
  | n == 1 = x
  | even n = expOpt (x*x) m
  | odd n  = x * expOpt (x*x) m
  where m = n `div` 2

expOpt2 = expOpt 2 

main :: IO()
main = defaultMain 
  [
    bgroup "expon" [ bench "naive" $ whnf expNaiv2 1000
                   , bench "optim" $ whnf expOpt2  1000
                   ]
  ]
