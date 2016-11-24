  {-
    5*. Напишите функцию, в которой используется функционал
    dropWhile с предикатом ==, удаляющую элементы
    из начала списка.
  -}
  import Char (isSpace)

  {-
    The dropWhileEnd function drops the largest suffix
    of a list in which the given predicate holds for
    all elements. For example:

    > dropWhileEnd isSpace "foo\n" == "foo"
    > dropWhileEnd isSpace "foo bar" == "foo bar"
    > dropWhileEnd isSpace ("foo\n" ++ undefined) == "foo" ++ undefined

    Source: <https://hackage.haskell.org/package/extra-1.5.1/docs/Data-List-Extra.html#v:dropWhileEnd>
  -}
  dropWhileEnd :: (a -> Bool) -> [a] -> [a]
  dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

  {-
    Удаляет пробельные символы (isSimple c == True)
    из обоих концов данной строки s.
  -}
  trim :: String -> String
  trim s = dropWhileEnd isSpace  heading
    where heading = dropWhile isSpace s

  -- Неудачные тестовые примеры:

  test1 = all (\s -> trim s == "") [
          "", " ", "   ",
          "\n", "\n ", " \n",
          "\t", "\n\t", "\r \t"
        ]

  test2 = all (\s -> trim s == "abc") [
          "abc", " abc", "abc ",
          "\nabc", "abc\n", "\tabc\n \r"
        ]

  test3 = trim "ab\nc" == "ab\nc" &&
          trim "a bc " == "a bc"  &&
          trim "a\0\n" == "a\NUL" &&
          trim "\0abc" == "\NULabc"

  test = test1 && test2 && test3
