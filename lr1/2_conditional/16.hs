  -- Задание 16
  -- Задача FizzBuzz (Имран Гари). Напишите функцию, которая пе-
  -- чатает числа от 1 до 100,  но вместо чисел,  кратных трём, печатает
  -- "Fizz", вместо чисел,  кратных пяти,  - "Buzz", а если число кратно
  -- одновременно трём и пяти, то печатает "FizzBuzz".

  callback :: Int -> String
  callback n | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz"
             | n `mod` 3 == 0 = "Fizz"
             | n `mod` 5 == 0 = "Buzz"
             | otherwise      = show n

  fizzBuzz = map callback [1..100]

  checkCallback :: (Int, String) -> Bool
  checkCallback (n, s) = callback n == s

  fizz = "Fizz"
  buzz = "Buzz"

  test = all checkCallback [
            (1, "1")
          , (3, fizz)
          , (5, buzz)
          , (15, fizz ++ buzz)
          , (100, buzz)
        ]
