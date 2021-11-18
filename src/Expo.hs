{-# LANGUAGE ScopedTypeVariables #-}
module Expo where

import Numeric.Natural

expo :: Natural -> Natural -> Natural
expo b n = iter 1 b n
    where
        iter :: Natural -> Natural -> Natural -> Natural
        iter a b 0 = a
        iter a b n = iter (a * b) b (pred n)

expo' :: Natural -> Natural -> Natural
expo' b n
    | n == 0    = 1
    | even n    = square (expo b (n `div` 2))
    | otherwise = b * expo b (pred n)

square :: Natural -> Natural
square n = n * n

{-
      計算機
expo θ(n)
expo' θ(log n)
-}