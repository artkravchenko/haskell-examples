  {-
    11*. [Анисимов,Пупышев,2006,с.104,№10.42]
    Определим  последовательность строк Фибоначчи 
      s(1), s(2), ..., s(k), ...
    следующим образом:

    s(1) = 'b', s(2) = 'a', s(k) = s(k-1)s(k-2), k > 2.

    Тогда начало последовательности строк Фибоначчи выглядит так:
    b,a,ab,aba,abaab,abaababa,abaababaabaab.

    (а) Чему равна длина s(k)?
    (б) Дано k. Найдите s(k).
    (в) Дана строка s. Проверьте, не является ли она
    последовательностью Фибоначчи.
  -}

  -- there're troubles with import from external directories
  -- copied from "../../lr3/3/5.hs"
  fib2 :: (Integral a, Integral b) => a -> b
  fib2 1 = 1
  fib2 2 = 1
  fib2 k = it 3 1 1
    where
      it i p pp | i == k = current
                | True   = it (i + 1) current p
                where current = p + pp

  sfib :: Integral a => a -> String
  sfib 1 = "b"
  sfib 2 = "a"
  sfib k = it 3 (sfib 2) (sfib 1)
    where
      it i p pp | i == k = current
                | True   = it (i + 1) current p
                where current = p ++ pp

  
  lengthSfib k = fib2 k

  isSfib1 :: String -> Bool
  isSfib1 s = it 1
    where
      slen = length s
      it k | c == s      = True
           | clen > slen = False
           | otherwise   = it (k + 1)
           where c = sfib k; clen = length c

  isSfib2 :: String -> Bool
  isSfib2 s | slen == 1 = s == fst || s == snd
            | otherwise = it snd fst
            where
              slen = length s
              fst = sfib 1; snd = sfib 2
              it p pp | c == s      = True
                      | clen > slen = False
                      | otherwise   = it c p
                      where c = p ++ pp; clen = length c; 

  examples = [
                "b",
                "a",
                "ab",
                "aba",
                "abaab",
                "abaababa",
                "abaababaabaab"
              ]

  trueTestSuite f  = all f examples
  falseTestSuite f = all (\s -> f == False) examples

  test1 = map sfib [1..7] == examples
  test =  test1 &&
          trueTestSuite isSfib1 &&
          trueTestSuite isSfib2 &&
          falseTestSuite isSfib1 &&
          falseTestSuite isSfib2
