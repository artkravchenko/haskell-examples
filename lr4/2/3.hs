  {-
    3. Напишите  функцию,  меняющую местами пару элементов списка по
       их указанным индексам.
  -}

  {-
    Меняет местами пару элементов данного списка lst по
    их указанным индексам x и y.
  -}
  swap :: Int -> Int -> [a] -> [a]
  swap x y lst | x >= len || y >= len =
      error ("swap " ++ (show x) ++ " " ++ (show y) ++ " lst - indexes are too large")
               | x == y               = lst
               | otherwise            =
      begin ++ [lst !! maxI] ++ mid ++ [lst !! minI] ++ end
    where
      len = length lst
      minI = min x y
      maxI = max x y
      begin = take minI lst
      mid = drop (minI + 1) (take maxI lst)
      end = drop (maxI + 1) lst

  -- Неудачные тестовые примеры:

  {-
    Не работает:
    testCasesArgs = [
                      ((0, 0), [1]),
                      ((1, 2), [1, 2, 3]),
                      ((0, 2), [1, 2, 3])
                    ]
    test = map (uncurry $ uncurry swap) (map (fst.head) testCasesArgs)
                                                        ^^^^^^^^^^^^^^
  -}

  -- Преобразует swap из
  -- :: Int -> Int -> [a] -> [a] в
  curriedSwap :: ((Int, Int), [a]) -> [a]
  curriedSwap = uncurry $ uncurry swap

  -- для [a], где a - целочисленный тип
  testNum = map curriedSwap [
              ((0, 0), [1]),
              ((0, 1), [1, 2]),
              ((1, 2), [1, 2, 3]),
              ((0, 2), [1, 2, 3]),
              ((2, 2), [1, 2, 3]),
              ((1, 2), [1, 2, 3, 4])
            ] == [
              [1],
              [2, 1],
              [1, 3, 2],
              [3, 2, 1],
              [1, 2, 3],
              [1, 3, 2, 4]
            ]

  -- для [a] == String
  testStr1 = map curriedSwap [
              ((0, 0), "a"),
              ((0, 1), "if"),
              ((0, 1), "fox"),
              ((0, 4), "hello")
            ] == [
              "a",
              "fi",
              "ofx",
              "oellh"
            ]

  -- Применение swap дважды с одинаковыми индексами-аргументами
  -- возвращает список, равный исходному
  testStr2 = swap 0 4 (swap 0 4 "hello") == "hello"

  test = testNum && testStr1 && testStr2
