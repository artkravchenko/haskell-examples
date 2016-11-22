  {-
    12*. [Анисимов,Пупышев,2006,с.146,№13.3]
    Можно ли разбить заданную строку на две части так, чтобы,
    переставив их местами, получить результирующую?
  -}

  {-
    eas -- equal after swap
    Проверяет, возможно ли разбить список xs на две части
    так, чтобы, переставив их местами, получить список ys.
  -}
  eas :: (Eq a) => [a] -> [a] -> Bool
  eas [] [] = True
  eas xs ys =
    let  lx = length xs; ly = length ys
    in case () of
      _ | lx /= ly  -> False
        | otherwise -> it 0
        where
          it n | n == lx    = False
               | heq && deq = True
               | otherwise  = it (n + 1) 
               where
                 spl = splitAt n xs
                 heq = (head $ snd spl)   == head ys -- head equality
                 deq = snd spl ++ fst spl == ys      -- raw equality

  -- Неудачные тестовые примеры:

  {-
    test0 = eas [] [] == True
  
    1. При чтении с помощью интерпретатора (hugs):

      ERROR "12.hs":27 - Unresolved top-level overloading
      *** Binding             : test0
      *** Outstanding context : Eq b
    
    2. При ручном вводе в течение сессии repl (hugs) работает:
    
      > eas [] []
      True 
  -}

  -- разная длина 
  test1 = eas [1..4] [1..3] == False
  
  -- идентичные непустые списки
  test2 = eas [1..3] [1..3] == True

  test3 = eas [2, 3, 1] [1..3]                == True &&
          eas [5, 3, 4, 1, 2] [1, 2, 5, 3, 4] == True &&
          -- ys и snd списки не идентичны (строка 24)
          eas [5, 3, 4, 1, 3] [1, 2, 5, 3, 4] == False &&
          eas [1, 1, 5, 4] [4, 1, 1, 5]       == True

  test = {- test0 && -} test1 && test2 && test3
