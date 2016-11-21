  -- Задание 15
  -- Сравните  приведённые ниже функции с точки зрения количества сравнений:

  -- 3
  min3 x y z = if x<=y 
                 then if x<=z then x else z
                 else if y<=z then y else z

  -- 5?
  min3' x y z = if x<=y && x<=z
                  then x
                  else if y<=z then y else z
