  {-
    4*. Напишите  функцию,  которая по двум заданным
    спискам выводит единый список, элементы которого
    образованы путём сложения элементов исходных списков,
    стоящих на одинаковых позициях.
  -}

  summarize :: Num a => [a] -> [a] -> [a]
  summarize lst1 lst2 = iterate [] lst1 lst2
    where
      iterate acc [] r2         = acc
      iterate acc r1 []         = acc
      iterate acc (x:xs) (y:ys) = iterate (acc ++ [x + y]) xs ys

  -- Неудачные тестовые примеры:

  test1 = all (\p -> (uncurry $ summarize) p == []) [
            ([], []), ([1], []), ([], [1]),
            ([1, 2], []), ([], [1, 2])
          ]

  test2 = summarize [1] [1]    == [2] &&
          summarize [1] [-1]   == [0] &&
          summarize [1, 3] [1] == [2] &&
          summarize [1] [1, 3] == [2]

  test = test1 && test2
