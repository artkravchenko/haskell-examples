  -- Задание 2.4
  -- Вариант 6

  f :: Floating a => a -> a -> Maybe a
  f x y | x == y = Nothing
        | True   = Just ((2*x - y) / (x - y))

  check :: Floating a => (a, a, Maybe a) -> Bool
  check (x, y, r) = f x y == r

  test = all check [
            (1, 1, Nothing)
          , (8, 4, Just 3)
          , (-5, -0.5, Just (9.5 / 4.5))
        ]
