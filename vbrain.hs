import Lib.Interpreter
import Lib.Memory
import Control.Monad
import Text.Parsec
import System.Environment

main = do
  (file:fs) <- getArgs
  f <- readFile file
  putStrLn $ show $ vbrain f
