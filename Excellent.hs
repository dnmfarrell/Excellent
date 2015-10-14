module Excellent where

intLen :: Int -> Int
intLen x = ceiling $ logBase 10 $ fromIntegral y
  where y = if x `mod` 10 == 0 then x + 1 else x

intSplit :: Int -> Maybe (Int, Int, Int)
intSplit x
  | x < 10    = Nothing
  | odd l     = Nothing
  | otherwise = Just (x, top, bottom)
  where l      = intLen x
        h      = ceiling (fromIntegral l / 2)
        bottom = x `mod` (10^h)
        top    = (x - bottom) `div` (10^h)

intExcellent :: Maybe (Int,Int,Int) -> Bool
intExcellent Nothing        = False
intExcellent (Just (x,y,z)) = x == ((z*z) - (y*y))
