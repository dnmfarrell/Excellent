import Excellent
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  print $ filter intExcellent [1..]
