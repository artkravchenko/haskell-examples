module Examples
    (
      acot'
    , atan'
    ) where

atan' :: Double -> Double
atan' x
  | x < 0 = -atan' (-x)
  | x > 1 = pi / 2 - atan' (1 / x)
  | otherwise =
      iterate x 0 -- Taylor series for atan
    where
      n' = 30
      iterate :: Double -> Int -> Double
      iterate x' n
          | n == n' = 0
          | otherwise =
              (-1) ^ n * x' / fromIntegral (2 * n + 1)
              + iterate (x' * x * x) (n + 1)

-- NOTE: E(acot'x) = (0; pi)
acot' :: Double -> Double
acot' x
  | x < 0     = pi - acot' (-x)
  | otherwise = pi / 2 - atan' x
