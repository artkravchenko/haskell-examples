  {-
    10*. Напишите функцию, моделирующую функционал map.
  -}
  map' :: (a -> b) -> [a] -> [b]
  map' f lst = iterate [] lst
    where
      iterate acc []     = acc
      iterate acc (x:[]) = acc ++ [f x]
      iterate acc (x:xs) = iterate (acc ++ [f x]) xs

  -- Неудачные тестовые примеры:

  -- Проверяет равенство результатов применения map
  -- и map' к одним и тем же входным параметрам
  check :: Eq b => ((a -> b), [a]) -> Bool
  check (f,lst) = map f lst == map' f lst

  test = all check [
          (id, []),
          (id, [1, 2, 3]),
          -- ((== 0), [1, 2, 0]),
          ((* 3), [1, 3, 8])
        ]
