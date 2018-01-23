
module Memory(
  Memory,
  regVal,
  goLeft,
  goRight,
  incReg,
  decReg, 
  ram
) where


--ADT for our representation of memory and various functions that act on it
data Memory = Null | Register Int Memory Memory deriving (Eq)

instance Show Memory where
  show Null = ""
  show (Register value left right) = (show left) ++ " " ++ (show value) ++ " " ++ (show right)

regVal :: Memory -> Int
regVal Null = 0
regVal (Register n _ _) = n

goLeft :: Memory -> Memory
goLeft (Register value left right)
  | left == Null = (Register 0 Null (Register value left right))
  | otherwise    = left

goRight :: Memory -> Memory
goRight (Register value left right)
  | right == Null = (Register 0 (Register value left right) Null)
  | otherwise     = right

incReg :: Memory -> Memory
incReg (Register x left right) = (Register (x + 1) left right)
incReg Null = Null

decReg :: Memory -> Memory
decReg (Register x left right) = (Register (x - 1) left right)
decReg Null = Null

--starting point for all memory manipulation
ram = (Register 0 Null Null)
