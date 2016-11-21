  -- Задание 2.12
  -- Напишите функцию, которая возвращает по заданному числу месяца
  -- и по дню недели первого числа этого месяца день недели для
  -- заданного данного числа.

  dayFromNum :: Int -> String
  dayFromNum n = case n of
    0 -> "понедельник"
    1 -> "вторник"
    2 -> "среда"
    3 -> "четверг"
    4 -> "пятница"
    5 -> "суббота"
    6 -> "воскресенье"

  numFromDay :: String -> Int
  numFromDay d = case d of
    "понедельник" -> 0
    "вторник"     -> 1
    "среда"       -> 2
    "четверг"     -> 3
    "пятница"     -> 4
    "суббота"     -> 5
    "воскресенье" -> 6

  getDayName :: Int -> String -> String
  getDayName (n + 1) firstDayTitle =
    dayFromNum $ ((numFromDay (firstDayTitle) + n) `mod` 7)


  check :: (Int, String, String) -> Bool
  check (n, firstDayTitle, r) = getDayName n firstDayTitle == r

  dayFromNumSpec = all (\(n, d) -> dayFromNum n == d) [
                      (0, "понедельник")
                    , (1, "вторник")
                    , (2, "среда")
                    , (3, "четверг")
                    , (4, "пятница")
                    , (5, "суббота")
                    , (6, "воскресенье")
                  ]

  test = all check [
            (1, "понедельник", "понедельник")
          , (6, "пятница", "среда")
        ]
