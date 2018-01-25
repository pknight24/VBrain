module Lib.Interpreter (
  whitespace,
  getOp,
  ignoreChars,
  convertOp,
  evalOp,
  eval,
  vbrain
  ) where

import Lib.Memory
import Text.Parsec
import Text.Parsec.String
import System.IO.Unsafe

--makes whitespace irrelevant
whitespace :: Parser ()
whitespace = skipMany $ oneOf " \n\t"

--all the possible chars in our brainfuck interpreter
getOp :: Parser Char
getOp = do
  whitespace
  ignoreChars 
  op <- oneOf "+-<>[.,"
  whitespace
  ignoreChars
  return op

getLoop = do
  ops <- many1 getOp
  char ']'
  return ops


--brainfuck ignores all characters that aren't operations
ignoreChars = skipMany $ noneOf "+-<>[].,"

--functions that the op's correspond to 
convertOp :: Char -> (Memory -> Memory)
convertOp '+' = incReg
convertOp '-' = decReg
convertOp '<' = goLeft
convertOp '>' = goRight
convertOp '.' = showMe

--gets the resulting memory for the loop function
loopResult :: Either ParseError Memory -> Memory
loopResult (Right m) = m

--begins the loop by parsing for the loop operations
initLoop :: Memory -> Parser Memory
initLoop m = do
  ops <- getLoop
  loop m ops

--takes an initial memory and a string and loops through these operations until the starting memory is zero
loop :: Memory -> String -> Parser Memory
loop m s = do
  case isZero m of
    True  -> return m
    False -> loop (loopResult $ parse (eval m) "looping" s) s
    

evalOp m = do
  op <- getOp
  case op of
    '[' -> initLoop m
    ',' -> return m
    _   -> return ((convertOp op) m)
          

eval m = do
  r <- evalOp m
  eval r <|> return r


vbrain input = parse (eval ram) "vbrain-eval" input
