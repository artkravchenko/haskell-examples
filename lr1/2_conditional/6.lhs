  Задание 2.6

  a    - величина стороны квадрата
  m, n - величины сторон прямоугольника

> greater :: Real a => a -> a -> a -> Maybe String
> greater a m n | a <= 0 =
>                   if m <= 0 || n <= 0
>                     then Nothing
>                     else Just "Rectangle"
>               | m <= 0 || n <= 0 =
>                   if a <= 0
>                     then Nothing
>                     else Just "Square"
>               | a*a > m*n && 2*a > m + n = Just "Square"
>               | True = Just "Rectangle"

  g - greater - большая из фигур

> check :: Real a => (a, a, a, Maybe String) -> Bool
> check (a, m, n, g) = greater a m n == g

> square = Just "Square"
> rect = Just "Rectangle"

> test = all check [
>           (0, 0, 0, Nothing)
>         , (0, 1, 0, Nothing)
>         , (0, 0, 0, Nothing)
>         , (-1e3, 0, 0, Nothing)
>         , (-1e3, 1, 0, Nothing)
>         , (-1e3, 0, 1, Nothing)
>         , (0, -1e-3, -1e5, Nothing)
>         , (0, -1e-3, 10, Nothing)
>         , (1, 0, 0, square)
>         , (0, 1, 10, rect)
>         , (0, 1, 1e-5, rect)
>         , (2, 1, 4, rect)
>         , (3, 2, 4, rect)
>         , (3, 1, 3, square)
>       ]
