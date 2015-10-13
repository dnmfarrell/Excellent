module Excellent where

intLen :: Int -> Int
intLen x = ceiling $ logBase 10 $ fromIntegral y
  where y = if x `mod` 10 == 0 then x + 1 else x

intSplit :: Int -> Maybe (Int, Int)
intSplit x
  | odd l || x < 11 = Nothing
  | otherwise       = Just (top, bottom)
  where l      = intLen x
        h      = ceiling (fromIntegral l / 2)
        bottom = x `mod` (10^h)
        top    = (x - bottom) `div` (10^h)

intExcellent :: Int -> Bool
intExcellent x
  | intSplit x == Nothing = False
  | x == (abs $ (a*a) - (b*b)) = True
  | otherwise    = False
  where Just (a,b) = intSplit x
