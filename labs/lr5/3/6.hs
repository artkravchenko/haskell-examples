  {-
    6. Напишите функцию,  обращающую каждый элемент
    списочной структуры (т.е. списки), а также саму
    списочную структуру. Например:
  
    [1,2,3,4,5,6,7,8,9] -> [9,8,7,6,5,4,3,2,1],
    [[1,2],[3],[4,5,6]] -> [[6,5,4],[3],[2,1]],
    [[[1,(-2),(-5),10]],[[(-3),(-2),1]]]
                      -> [[[1,(-2),(-3)]],[[10,(-5),(-2),1]]].
  -}
  
  -- import Prelude hiding ( reverse )

  {-
    Attempt #1

    ERROR "6.hs":22 - Type error in function binding
    *** Term           : reverse
    *** Type           : [a] -> [a]
    *** Does not match : a -> a
    *** Because        : unification would give infinite type

    reverse lst = it [] lst
      where
        isList x = head (show x) == '['
        it acc []     = acc
        it acc (x:[]) | isList x  = ((reverse x) : acc)
                      | otherwise = x : acc
        it acc (x:xs) | isList x  = it ((reverse x) : acc) xs
                      | otherwise = it (x : acc) xs
  -}

  {-
    Attempt #2

    ERROR "6.hs":42 - Type error in function binding
    *** Term           : reverse
    *** Type           : [[a]] -> [[a]]
    *** Does not match : [a] -> [a]
    *** Because        : unification would give infinite type


    reverse lst = it [] lst
      where
        it acc []           = acc
        it acc (x@(_:_):[]) = ((reverse x) : acc)
        it acc (x      :[]) = x : acc
        it acc (x@(_:_):xs) = it ((reverse x) : acc) xs
        it acc (x      :xs) = it (x : acc) xs
  -}


  {-
    Attempt #3

    List - тип данных, предназначенный для хранения
    вложенных списков.

    Примечание: на самом деле является типом, задающим
    дерево, где
      * Elem - лист (Leaf),
      * Sublist - узел, ветка (Node)
    
    Конструктор `Elem a` определен для элементов списка,
    не являющимися списками.
    Конструктор `Sublist [List a]` предназначен для описания
    списочных структур, в т. ч. корневого списка.
  -}
  data List a = Elem a | Sublist [List a] deriving (Show)

  {-
    Преобразует обозначение List по-умолчанию
    к компактному аиду, имитируя встроенный в Plelude
    списочный тип данных.

    "Sublist [Sublist [Elem 1,Elem 2], Sublist []]" -> "[[1,2],[]]""
  -}
  toString :: Show a => List a -> String
  toString (Elem x)     = show x
  {-
    > toString (Sublist []) = show []

    ERROR "6.hs":66 - Cannot justify constraints in explicitly typed binding
    *** Expression    : toString
    *** Type          : Show a => List a -> String
    *** Given context : Show a
    *** Constraints   : Show b
    
    So we put "[]" manually
  -}
  toString (Sublist []) = "[]"
  toString (Sublist xs) = filter (/= '"') (show $ map toString xs)

  -- Инверсия вложенного списка.
  reverse' :: List a -> List a
  reverse' (Elem x) = Elem x
  reverse' (Sublist xs) = Sublist (reverse $ map reverse' xs)

  -- Неудачные тестовые примеры:
  
  testList1 = Sublist [
                Sublist [Elem 1, Elem 2, Elem 3],
                Sublist [Elem 4, Elem 5],
                Sublist []
              ]
  testList2 = Sublist [testList1, Sublist [
                Sublist [Elem 8, Elem 9, Elem 5],
                Sublist [Elem 3],
                Sublist [],
                Sublist [Elem 10, Elem 20]
              ]]

  test1 = (toString $ testList1) == "[[1,2,3],[4,5],[]]" &&
          (toString $ testList2) == "[" ++
                                      "[[1,2,3],[4,5],[]]," ++
                                      "[[8,9,5],[3],[],[10,20]]" ++
                                    "]"

  test2 = (toString $ reverse' testList1) == "[[],[5,4],[3,2,1]]" &&
          (toString $ reverse' testList2) == "[" ++
                                                "[[20,10],[],[3],[5,9,8]]," ++
                                                "[[],[5,4],[3,2,1]]" ++
                                              "]"

  test = test1 && test2
