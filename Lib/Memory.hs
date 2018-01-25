module Lib.Memory(
  Memory (..),
  regVal,
  goLeft,
  goRight,
  incReg,
  decReg, 
  isZero,
  showMe,
  ram
) where

import Data.Char

--ADT for our representation of memory and various functions that act on it
--includes an integer value, a register to the left, a register to the right, and a boolean value indicating 
----whether its integer value should be shown as a character
data Memory = Null | Register Int Memory Memory  Bool deriving (Eq)

--uses clearRight and clearLeft to ensure that duplicate memory locations are not printed
instance Show Memory where
  show Null = ""
  show (Register value left right False) = (show $ clearRight left) ++ " |" ++ (show value) ++ "| " ++ (show $ clearLeft right)
  show (Register value left right True) = (show $ clearRight left) ++ " |" ++ (show $ chr value) ++ "| " ++ (show $ clearLeft right)

showMe :: Memory -> Memory
showMe (Register v l r _) = (Register v l r True)

--clears the memory in the left register
clearLeft (Register v l r s) = (Register v Null r s)
clearLeft Null = Null

--clears the memory in the right register
clearRight (Register v l r s) = (Register v l Null s)
clearRight Null = Null

--gets the integer value at the current register
regVal :: Memory -> Int
regVal Null = 0
regVal (Register n _ _ _) = n

--gets register to the left of the current one
getLeft (Register _ left _ _) = left
getLeft Null = Null

--gets register to the right of the current one
getRight (Register _ _ right _) = right
getRight Null = Null

--moves the pointer to the left
goLeft :: Memory -> Memory
goLeft (Register value left right s)
  | left == Null = (Register 0 Null (Register value left right s) False)
  | otherwise    = (Register (regVal left) (getLeft left) (Register value left right s) False)

--moves the pointer to the right
goRight :: Memory -> Memory
goRight (Register value left right s)
  | right == Null = (Register 0 (Register value left right s) Null False)
  | otherwise     = (Register (regVal right) (Register value left right s) (getRight right) False)

--increments the current register by one
incReg :: Memory -> Memory
incReg (Register x left right s) = (Register (x + 1) left right s)
incReg Null = Null

--decrements the current register
decReg :: Memory -> Memory
decReg (Register x left right s) = (Register (x - 1) left right s)
decReg Null = Null

isZero :: Memory -> Bool
isZero m = (regVal m) == 0

--starting point for all memory manipulation
ram = (Register 0 Null Null False)
