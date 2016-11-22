  {-
    (По [Андерсон,2003,с.199-200])
    Напишите функцию, вычисляющую значения an по заданному
    рекурсивному определению:

    (6) {a(1) = 1;
        {a(2) = 1;
        {a(k) = a(k - 1) + a(k - 2), k > 1.
  -}

  fib1 :: (Integral a, Integral b) => a -> b
  fib1 1 = 1
  fib1 2 = 1
  fib1 k = fib1 (k - 1) + fib1 (k - 2)

  fib2 :: (Integral a, Integral b) => a -> b
  fib2 1 = 1
  fib2 2 = 1
  fib2 k = it 3 (fib2 2) (fib2 1)
    where
      it i p pp | i == k = current
                | True   = it (i + 1) current p
                where current = p + pp

  -- TODO: infinite fib list

  -- Неудачные тестовые примеры:
