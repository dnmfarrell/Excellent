import Excellent
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  print $ filter (\x -> intExcellent (intSplit x)) [1..]
