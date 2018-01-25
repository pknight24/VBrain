import Lib.Interpreter
import Lib.Memory
import Control.Monad
import Text.Parsec
import System.IO.Unsafe

main = forever $ do
  putStr "> "
  l <- getLine
  let m = vbrain l
  putStrLn (show m)
