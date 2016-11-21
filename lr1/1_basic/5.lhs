  Задание 1.5

> isLatinSymbol :: Char -> Bool;
> isLatinSymbol c | c >= 'a' = c <= 'z'
>                 | c >= 'A' = c <= 'Z'
>                 | True     = False

> test =    all isLatinSymbol (['A'..'Z'] ++ ['a'..'z'])
>        && all (\c -> not (isLatinSymbol c)) (['0'..'9'] ++ ['['..'`'])
