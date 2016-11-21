  -- Задание 3.11
  -- Напишите функцию, определяющую, является ли
  -- заданное натуральное число простым числом

  isSimple :: Int -> Bool
  isSimple n = iterate 2
    where
      iterate :: Int -> Bool
      iterate acc | (fromIntegral acc) > sqrt(fromIntegral n)   = True
                  | n `mod` acc == 0 = False
                  | otherwise        = iterate (acc + 1) 


  check :: (Int, Bool) -> Bool
  check (n, is) = isSimple n == is

  test = all check [
            (2, True)
          , (3, True)
          , (4, False)
          , (8, False)
          , (100, False)
          , (61, True)
        ]
