module Main where

import Examples (atan', acot')

main :: IO ()
main = do
  putStrLn ("atan 1.8 = " ++ (show $ atan' 1.8))
  putStrLn ("acot 1.8 = " ++ (show $ acot' 1.8))
  putStrLn ("atan (-1.8) = " ++ (show $ atan' (-1.8)))
  putStrLn ("acot (-1.8) = " ++ (show $ acot' (-1.8)))
