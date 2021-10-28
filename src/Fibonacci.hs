module Fibonacci where

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-
一歩で、一段または二段昇ることを考える
階段の10段目までの昇りかたは何通りあるか
fib 10 通りある。
答: fib 10 通りある。

               f5
           f4             f3
       f3    f2        f2     f1
   f2    f1  f1  f0   f1   f0   |
f1  f0  |     |   |    |    |   1
 |     |　　　1    1    1    1
  1     1     
-}
{- 樹状再起 tree recursion -}

-- fib2 (n-1) = (fib (n-2), fib (n-1))
-- fib2 n     = (fib (n-1), fib n)

{- 線形再帰プロセス -}
fib2 :: Integer -> (Integer, Integer)
fib2 1 =  (1, 1)
fib2 n = case fib2 (n-1) of
    (a, b) -> (b, a+b)

fib' :: Integer -> Integer
fib' n  = case fib2 n of
    (a, b) -> b
    
{- 線形反復プロセス-}
fib'' :: Integer -> Integer
fib'' 0 = 1
fib'' n = fibIter 1 1 n

fibIter :: Integer -> Integer -> Integer -> Integer
fibIter a b 1 = b
fibIter a b n = fibIter b (a+b) (n-1)