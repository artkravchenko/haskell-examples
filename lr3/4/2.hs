  -- Задание 4.2

  is :: Integer -> Double
  is a  | a < 0 = 0.0 -- Nothing
        | True  = iterate 0 (fromInteger a) 0
        where
          iterate :: Double -> Double -> Integer -> Double
          iterate acc prev i | i >= a = acc
                             | True   = iterate (acc + next) next (i + 1)
                             where
                               next = sqrt prev


  list = [2, 6, 12, 20, 30, 42, 56, 72, 90, 110, 132,
            156, 182, 210, 240, 272, 306, 342, 380, 420, 462, 992]

  test = all (\x -> is x == (1 + sqrt (fromInteger (1 + 4 * x))) / 2) list
