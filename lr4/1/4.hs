  {-
    4*. Напишите функцию,  определяющую, равен ли "центральный"
    элемент числового списка произведению крайних элементов.
  -}

  -- Определяет, равны ли значения аргументов, с точностью 10^-6
  eq :: (Ord a, Fractional a) => a -> a -> Bool
  eq a b = abs (a - b) <= 10 ^^ (-6)

  -- Опрелеляет, равен ли "центральный" элемент числового
  -- списка l произведению крайних элементов.
  check :: (Ord a, Fractional a) => [a] -> Bool
  check []  = error "check [] - List must be non-empty"
  check l   | even len = eq p (l !! (len `div` 2 - 1)) &&
                         eq p (l !! (len `div` 2))
            | True     = eq p (l !! (len `div` 2))
            where len = length l; p = head l * last l

  -- Неудачные тестовые примеры:
  
  test1a  = check [1] &&
            check [0] &&
            check [0.0] &&
            check [1.0] &&
            not (check [-1.0])

  test1b  = check [1, 1] &&
            check [0, 0] && 
            check [0.0, 0.0] &&
            check [1.0, 1.0] &&
            not (check [2, 2])

  -- погрешность
  test2 = check [1.00000001, 1.0, 1.0000001] &&
          not (check [0.001, 0.001]) &&
          not (check [1.00001, 1.00001])

  test3 = check [2, 4, 2] &&
          check [2.0, 8.0, 4.0] &&
          check [-8, -48, 6] &&
          check [6.0, -48.0, -8.0]

  test4 = check [1, 2, 2, 2] &&
          not (check [1, 3, 2, 2]) &&
          not (check [1, 2, 3, 2]) &&
          check [3.0, 15.0, 15.0, 5.0] &&
          check [-3.0, 15.0, 15.0, -5.0] &&
          not (check [-3.0, -15.0, 15.0, -5.0])

  test5 = check [25.0, 10.0 ^^ 5, 100.0, 1.0, 4.0]

  test6 = check [10^10, 10^20, 10^10] &&
          check [10.0 ^^ 10, 10.0 ^^ 20, 10.0 ^^ 10]

  test = test1a && test1b && test2 && test3 && test4 && test5 && test6

