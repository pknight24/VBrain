module Lib.Interpreter (
  whitespace,
  getOp,
  ignoreChars,
  convertOp,
  evalOp,
  eval,
  vbrain) where

import Lib.Memory
import Text.Parsec
import Text.Parsec.String
import Control.Monad.State

--makes whitespace irrelevant
whitespace :: Parser ()
whitespace = skipMany $ oneOf " \n\t"

--all the possible chars in our brainfuck interpreter
getOp :: Parser Char
getOp = oneOf "+-<>"

ignoreChars = skipMany $ noneOf "+-<>"

--functions that the op's correspond to 
convertOp :: Char -> (Memory -> Memory)
convertOp '+' = incReg
convertOp '-' = decReg
convertOp '<' = goLeft
convertOp '>' = goRight

evalOp m = do
  whitespace
  ignoreChars
  op <- getOp
  ignoreChars
  whitespace
  let o = convertOp op
  return (o m)

eval m = do
  r <- evalOp m
  eval r <|> return r

vbrain input = parse (eval ram) "vbrain-eval" input

