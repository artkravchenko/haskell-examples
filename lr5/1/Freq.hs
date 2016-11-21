  {-
    7*. (По [Анисимов,Пупышев,2006,с.68,№7.60])
    Прочитайте строку и сообщите, сколько раз
    встречается  нужный символ.

    Указание. Нужным символом называется символ,
    который встречается в заданной строке чаще других.
  -}
  module Freq ( freq ) where

  import Dupl ( dupl )

  freq :: String -> Int
  freq "" = 0
  freq s  = iterate (dupl s) 0
    where
      len = length s `div` 2
      iterate ((_,xn):[]) n = max xn n
      iterate ((_,xn):xs) n | xn > len  = xn
                            | xn > n    = iterate xs xn
                            | otherwise = iterate xs n

  test = Nothing
