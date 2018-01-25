module Lib.Memory(
  Memory (..),
  regVal,
  goLeft,
  goRight,
  incReg,
  decReg, 
  isZero,
  ram
) where


--ADT for our representation of memory and various functions that act on it
data Memory = Null | Register Int Memory Memory deriving (Eq)

--uses clearRight and clearLeft to ensure that duplicate memory locations are not printed
instance Show Memory where
  show Null = ""
  show (Register value left right) = (show $ clearRight left) ++ " |" ++ (show value) ++ "| " ++ (show $ clearLeft right)

--clears the memory in the left register
clearLeft (Register v l r) = (Register v Null r)
clearLeft Null = Null

--clears the memory in the right register
clearRight (Register v l r) = (Register v l Null)
clearRight Null = Null

--gets the integer value at the current register
regVal :: Memory -> Int
regVal Null = 0
regVal (Register n _ _) = n

--gets register to the left of the current one
getLeft (Register _ left _) = left
getLeft Null = Null

--gets register to the right of the current one
getRight (Register _ _ right) = right
getRight Null = Null

--moves the pointer to the left
goLeft :: Memory -> Memory
goLeft (Register value left right)
  | left == Null = (Register 0 Null (Register value left right))
  | otherwise    = (Register (regVal left) (getLeft left) (Register value left right))

--moves the pointer to the right
goRight :: Memory -> Memory
goRight (Register value left right)
  | right == Null = (Register 0 (Register value left right) Null)
  | otherwise     = (Register (regVal right) (Register value left right) (getRight right))

--increments the current register by one
incReg :: Memory -> Memory
incReg (Register x left right) = (Register (x + 1) left right)
incReg Null = Null

--decrements the current register
decReg :: Memory -> Memory
decReg (Register x left right) = (Register (x - 1) left right)
decReg Null = Null

isZero :: Memory -> Bool
isZero m = (regVal m) == 0

--starting point for all memory manipulation
ram = (Register 0 Null Null)
