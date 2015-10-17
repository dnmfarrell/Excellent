module Excellent where

intLen :: Int -> Int
intLen x = ceiling $ logBase 10 $ fromIntegral y
  where y = if mod x 10 == 0 then x + 1 else x

intSplit :: Int -> Maybe (Int, Int, Int)
intSplit x
  | x < 10    = Nothing
  | odd l     = Nothing
  | otherwise = Just (x, top, bottom)
  where l      = intLen x
        h      = div l 2
        bottom = mod x (10^h)
        top    = div (x - bottom) (10^h)

intExcellent :: Maybe (Int,Int,Int) -> Bool
intExcellent Nothing        = False
intExcellent (Just (x,y,z)) = x == ((z*z) - (y*y))

intRectangular :: Int -> Bool
intRectangular x = x == y * y - y
  where y = ceiling $ sqrt $ fromIntegral x

intExcellentTopHalf :: Int -> Bool
intExcellentTopHalf x = intRectangular (x * p + x)
  where k = intLen x
        p = 10^k
