  {-
    9. Напишите функции,  моделирующие операции сложения и умножения
    над комплексными числами.
  -}
  import Prelude hiding ( sum, product )

  -- Суммирует два комплексных числа a и b вида (x + y*i),
  -- записанные в виде (x, y).
  -- Возвращает комплексное число (x + y*i) в формате (x, y)
  sum :: Num a => (a, a) -> (a, a) -> (a, a)
  sum a b = (fst a + fst b, snd a + snd b)

  -- Перемножает два комплексных числа a и b вида (x + y*i),
  -- записанные в виде (x, y).
  -- Возвращает комплексное число (x + y*i) в формате (x, y)
  product :: Num a => (a, a) -> (a, a) -> (a, a)
  product a b = (
                  fst a * fst b - snd a * snd b,
                  fst a * snd b + snd a * fst b
                )
  
  -- Неудачные тестовые примеры:

  -- Определяет. равны ли значения числовых аргументов. с точностью 10^-6
  eq :: (Ord a, Fractional a) => a -> a -> Bool
  eq a b = abs (a - b) <= 10 ^^ (-6)

  -- Определяет. равны ли приближенно (с точностью 10^-6)
  -- значения соответствующих элементов двух аргументов-пар
  eqPair :: (Ord a, Fractional a) => (a, a) -> (a, a) -> Bool
  eqPair a b = eq (fst a) (fst b) && eq (snd a) (snd b)

  testSum =
    eqPair (sum (0, 0) (0, 0)) (0, 0) &&
    eqPair (sum (0.1, 0.1) (-0.1, -0.1))                     (0.0, 0.0) &&
    eqPair (sum (0.000001, 0.000001) (-0.000001, -0.000001)) (0.0, 0.0) &&
    eqPair (sum (10^10, 10^20) (10^10, 10^20))       (2 * 10^10, 2 * 10^20)

  testProduct =
    eqPair (product (0.0, 0.0) (0.0, 0.0)) (0.0, 0.0) &&
    eqPair (product (0, 0) (0, 0))         (0, 0) &&
    eqPair (product (7, 3) (5, -8))        (59, -41) &&
    eqPair (product (-10, -4) (8, 1))      (-76, -42) &&
    eqPair (product (12, 0) (6, 2))        (72, 24) &&
    eqPair (product (0, 9) (0, -10))       (90, 0)
    
  test = testSum && testProduct
