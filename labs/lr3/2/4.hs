  {-
    4*. Напишите функцию, вычисляющую значение функции
    при n, m <- N:

    (1) F(n) = 4^n;
    (2) F(n) = n^n;
    (3) F(m,n) = m^n.
  -}
  

  -- (3)
  powMN :: (Integral a, Integral b) => a -> b -> a
  powMN m n = iterate 1 n
    where
      iterate acc 0 = acc
      iterate acc i = iterate (acc * m) (i - 1)

  -- (1)
  pow4 :: (Integral a, Integral b) => b -> a
  pow4 = powMN 4

  -- (2)
  powN :: Integral a => a -> a
  powN n = iterate 1 n
    where
      iterate acc 0 = acc
      iterate acc i = iterate (acc * n) (i - 1)

  -- Неудачные тестовые примеры:
  
  pow4Test = take 100 (map pow4 [0..]) == take 100 (map (4^) [0..])

  powNTest = take 100 (map powN [0..]) == take 100 (map (\n -> n ^ n) [0..])

  powMNTest1 = take 10000 (map (uncurry powMN) lst) == take 10000 (map (\(m, n) -> m ^ n) lst)
    where lst = [(m, n) | m <- [1..], n <- [0..50]] 

  -- t ~ 1.5 seconds
  powMNTest2 = all (uncurry (==)) (take 10000 pairs)
    where
      pairs = map (\(m,n) -> (powMN m n, m ^ n)) lst
      lst = [(m, n) | m <- [1..], n <- [0..50]]

  -- t ~ 1.8 seconds
  powMNTest3 :: Integral a => a -> Bool
  powMNTest3 num = iterate num 1 0
    where
      check m n = powMN m n == m ^ n
      iterate 0 m n = True
      iterate i m 50 | check m 50 = iterate (i - 1) (m + 1) 0
                     | otherwise  = False
      iterate i m n  | check m n  = iterate (i - 1) m (n + 1)
                     | otherwise  = False
