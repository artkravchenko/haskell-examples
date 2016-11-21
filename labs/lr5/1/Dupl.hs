  {-
    5. Напишите функцию,  возвращающую список пар,  первым элементом
    которых является атом, а вторым - количество вхождений его в список
  -}
  module Dupl ( dupl ) where

  import Maybe

  dupl :: (Eq a, Integral b) => [a] -> [(a, b)]
  dupl [] = []
  dupl lst@(x:xs) = iterate [(x, 1)] xs
    where
      -- iterates through list
      iterate acc [] | llen == alen = acc
                     | isJust ady   = iterate (acc ++ [(hr, fromJust ady)]) [] -- not to iterate
                     | otherwise    = iterate (acc ++ [(hr, 1)]) (drop (alen + 1) lst)
                    where
                      llen = length lst
                      alen = length acc
                      hr   = lst !! alen   -- head of rest
                      ady  = lookup hr acc -- already counted
      iterate acc r  | fst c == hr  = iterate (init acc ++ [(fst c, snd c + 1)]) $ tail r
                     | otherwise    = iterate acc $ tail r
                    where
                      c   = last acc -- currently counted
                      hr  = head r   -- head of rest
  
  test = Nothing
