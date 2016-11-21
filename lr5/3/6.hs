  {-
    6. Напишите функцию,  обращающую каждый элемент
    списочной структуры (т.е. списки), а также саму
    списочную структуру. Например:
  
    [1,2,3,4,5,6,7,8,9] -> [9,8,7,6,5,4,3,2,1],
    [[1,2],[3],[4,5,6]] -> [[6,5,4],[3],[2,1]],
    [[[1,(-2),(-5),10]],[[(-3),(-2),1]]]
                      -> [[[1,(-2),(-3)]],[[10,(-5),(-2),1]]].
  -}
  import Prelude hiding ( reverse )

  reverse :: [a] -> [a]
  reverse lst = iterate [] lst
    where
      isList x = head (show x) == '['
      iterate :: [a] -> [a] -> [a]
      iterate acc []     = acc
      iterate acc (x:[]) | isList x  = ((reverse x) : acc)
                         | otherwise = x : acc
      iterate acc (x:xs) | isList x  = iterate ((reverse x) : acc) xs
                         | otherwise = iterate (x : acc) xs


