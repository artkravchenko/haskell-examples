  -- import qualified Lab (getSumOfDigits)
  import Data.Maybe
  import Control.Applicative

  instance Num a => Num (Maybe a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger = pure . fromInteger

  getSumOfDigits :: Integral a => a -> Maybe a
  getSumOfDigits n  | n >= 1000 = Nothing
                    | otherwise =
                        Just (iterate 0 (abs n))
                      where
                        iterate :: Integral a => a -> a -> a
                        iterate acc 0  = acc
                        iterate acc n' = iterate (acc + n' `mod` 10) (n' `div` 10)

  -- Задание 11.
  -- Напишите функцию, подсчитывающую число рождения, которым
  -- называется сумма всех цифр указанной даты.

  getBirthNumber :: Int -> Int -> Int -> Maybe Int
  getBirthNumber d m y =
    -- year starts with 1 as historical year
    if (any (\x -> x < 1) [d, m, y])
      then Nothing
      else
        let
          d'  = getSumOfDigits d
          m'  = getSumOfDigits m
          y1' = getSumOfDigits (y `div` 100)
          y2' = getSumOfDigits (y `mod` 100)
        in
          if (any isNothing [d', m', y1', y2'])
            then Nothing
            else d' + m' + y1' + y2'

  check :: (Int, Int, Int, Maybe Int) -> Bool
  check (d, m, y, num) = getBirthNumber d m y == num

  test = all check [
            (00, 00, 0000, Nothing)
          , (00, 00, 0001, Nothing)
          , (00, 01, 0001, Nothing)
          , (01, 01, 0001, Just 3)
          , (01, 01, 1000, Just 3)
          , (20, 12, 1550, Just 16)
        ]
