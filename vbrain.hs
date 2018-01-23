import Lib.Interpreter
import Lib.Memory
import Control.Monad

main = forever $ do
  putStr "> "
  l <- getLine
  let m = vbrain l
  putStrLn (show m)
