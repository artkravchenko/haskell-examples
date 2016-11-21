  {-
    (По [Андерсон,2003,с.199-200])
    Напишите функцию, вычисляющую значения an по заданному
    рекурсивному определению:

    (6) {a(1) = 1;
        {a(2) = 1;
        {a(k) = a(k - 1) + a(k - 2), k > 1.
  -}

  fib1 :: (Ord a, Integral a, Integral b) => a -> b
  fib1 1 = 1
  fib1 2 = 1
  fib1 k = fib1 (k - 1) + fib1 (k - 2)

  --fib2 1 = 1
  --fib2 2 = 1
  --fib2 n = iterate n 1 1
  --  where
  --    iterate acc l ll = 
