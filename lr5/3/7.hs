  {-
    7. Напишите  функцию,  удаляющую  из  списка
    повторные вхождения элементов.
  -}

  uniq :: Eq a => [a] -> [a]
  uniq lst@(x:xs) = it [x] xs
    where
      it acc []     = acc
      it acc (y:[]) | notElem y acc = acc ++ [y]
                    | otherwise     = acc
      it acc (y:ys) | notElem y acc = it (acc ++ [y]) ys
                    | otherwise     = it acc ys

  -- Неудачные тестовые примеры:

  test = Nothing
