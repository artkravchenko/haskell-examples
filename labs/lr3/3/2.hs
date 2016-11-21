  {-
    2*. Напишите на языке Haskell функцию "91-МакКарти", заданную
    следующим образом:

    F(n)=If n>100
      then n-10
      else F(F(F(n+21)))
  -}

  mc91 :: (Ord a, Num a) => a -> a
  mc91 n | n > 100   = n - 10
         | otherwise = mc91 $ mc91 $ mc91 $ n + 21

  -- Вариант функции с использованием хвостовой рекурсии
  mc91tail :: (Ord a, Num a) => a -> a
  mc91tail n = iterate n 1
    where
      iterate a i | i == 0    = a
                  | a > 100   = iterate (a - 10) (i - 1)
                  | otherwise = iterate (a + 21) (i + 2)

  -- Неудачные тестовые примеры:

  -- Жди глубокую рекурсию!
  check0 f = all (\n -> f (-10 ^ n) == 91)
  
  check1 f = all (\n -> f n == 91)
  check2 f = all (\n -> f n == n - 10)

  test1 = check1 mc91 [-100..100] &&
          check2 mc91 [101..1000]

  test2 = check0 mc91tail [0..6] &&
          check1 mc91tail [-1000..100] &&
          check2 mc91tail [101..1000]

