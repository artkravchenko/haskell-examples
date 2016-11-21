  Задание 2.5
  Напишите функцию, возвращающую наибольшее из трёх чисел.

> max3 :: Ord a => a -> a -> a -> a
> max3 a b c | a > b && a > c = a
>            | b > c = b
>            | True  = c


> check :: Ord a => (a, a, a, a) -> Bool
> check (a, b, c, m) = max3 a b c == m

> test = all check [
>           (3, 5, 7, 7)
>         , (-2, -6, -1, -1)
>         , (1e-8, 1e-12, 1e-9, 1e-8)
>         , (1e9, 1e10, 1e13, 1e13)
>       ]
