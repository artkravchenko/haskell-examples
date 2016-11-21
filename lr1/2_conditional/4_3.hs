  -- Задание 2.4
  -- Вариант 3

  f :: Floating a => a -> a -> Maybe a
  f x y | x == 2*y = Nothing
        | True     = Just ((x - y) / (x - 2*y))

  check :: Floating a => (a, a, Maybe a) -> Bool
  check (x, y, r) = f x y == r

  test = all check [
            (1, 0.5, Nothing)
          , (8, 2, Just 1.5)
          , (-5, -0.5, Just 1.125)
        ]
