  -- module Lab
  --   ( getSumOfDigits ) where

  -- Задание 2.10
  -- Напишите  функцию,  определяющую  сумму  цифр 
  -- целого  числа n < 1000.

  getSumOfDigits :: Integral a => a -> Maybe a
  getSumOfDigits n  | n >= 1000 = Nothing
                    | otherwise =
                        Just (iterate 0 (abs n))
                      where
                        iterate :: Integral a => a -> a -> a
                        iterate acc 0  = acc
                        iterate acc n' = iterate (acc + n' `mod` 10) (n' `div` 10)

  check :: Integral a => (a, Maybe a) -> Bool
  check (n, sum) = getSumOfDigits n == sum

  test = all check [
            (1000, Nothing)
          , (1001, Nothing)
          , (truncate 1e8, Nothing)
          , (999, Just 27)
          , (900, Just 9)
          , (910, Just 10)
          , (901, Just 10)
          , (111, Just 3)
          , (10, Just 1)
          , (1, Just 1)
          , (0, Just 0)
          , (-50, Just 5)
          , (-999, Just 27)
          , (-1000, Just 1)
          , (-1001, Just 2)
          , (-123456789, Just 45)
        ]
