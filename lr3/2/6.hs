  {-
    6. (Душкин Р.В.) Напишите программу, вычисляющую значение
    следующих функций при значениях n <- N\{0}:

    (1) F(n) = E(k=1, n) k;  
  -}

  fun1 :: Integral a => a -> a
  fun1 n = iterate 0 1
    where iterate acc k | k > n = acc
                        | True  = iterate (acc + k) (k + 1)

  -- too slow: > 3 minutes
  fun1Test1 num = all (uncurry (==)) (take num pairs)
    where pairs = map (\n -> (fun1 n, sum [0..n])) [0..]

  fun1Test2 n = iterate 0 1
    where iterate prev k | k > n              = True
                         | fun1 k == prev + k = iterate (prev + k) (k + 1)
                         | otherwise          = False

