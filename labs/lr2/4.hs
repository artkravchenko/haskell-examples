  -- 4. Вычислите число п с помощью тригонометрических функций.

  pi1 = acos (-1)
  pi2 = asin 1 * 2
  pi3 = atan 1 * 4

  -- Вычисляет arcctg x
  acot :: Floating a => a -> a
  acot x = pi / 2 - atan x

  pi4 = acot 1 * 4

  -- Неудачные тестовые примеры:

  -- Определяет, равны ли значения аргументов, с точностью 10^-6
  eq :: (Ord a, Fractional a) => a -> a -> Bool
  eq a b = abs (a - b) <= 10 ^^ (-6)

  -- Вычисляет базовые выражения с использованием
  -- тригонометрических ф-ций и данного п (p) и
  -- сравнивает результаты с "каноническими" решениями
  check :: (Ord a, Floating a) => a -> Bool
  check p = eq (cos p)      (-1.0) &&
            eq (cos (2 * p)) 1.0   &&
            eq (sin p)       0.0   &&
            eq (sin (2 * p)) 0.0

  test1 = all check [pi1, pi2, pi3, pi4]

  -- Проверяет приближенное равенство вычисленных значений п
  -- и pi из Prelude.
  test2 = all (eq pi) [pi1, pi2, pi3, pi4]

  test = test1 && test2
