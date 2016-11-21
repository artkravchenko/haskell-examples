  {-
    3. Напишите функцию от трёх аргументов, включающую
    заданный элемент в список на указанную позицию.
  -}
  insert :: Int -> a -> [a] -> [a]
  insert p e lst | p >= length lst = error err
                 | otherwise       = take p lst ++ [e] ++ drop p lst
                 where err = "insert: Position is out of range."

  replace :: Int -> a -> [a] -> [a]
  replace p e lst | p >= length lst = error err
                  | otherwise       = take p lst ++ [e] ++ drop (p + 1) lst
                  where err = "replace: Position is out of range."

  test = Nothing
