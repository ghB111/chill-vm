module Compiler ( compile ) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

import Vm
import Parser

-- instructions by number of arguements
data InstructionT = 
    Instruction0 Instruction |
    Instruction1 (Int -> Instruction) |
    Instruction2 (Int -> Int -> Instruction)

instance Show InstructionT where
    show (Instruction0 i) = show i
    show (Instruction1 i1) = show $ i1 0
    show (Instruction2 i2) = show $ i2 0 0

bitcodeToInstruction :: String -> InstructionT
bitcodeToInstruction str = case str of
    "00000000" -> Instruction0 Chl
    "00000001" -> Instruction2 Ldc
    "00000010" -> Instruction1 Nul
    "00000011" -> Instruction2 Ld
    "00000100" -> Instruction0 Sgf
    "00000101" -> Instruction1 Jmp
    "00000110" -> Instruction2 Pls
    "00000111" -> Instruction2 Mns
    "00001000" -> Instruction1 Zbr
    "00001001" -> Instruction1 Bbr
    "00001010" -> Instruction1 Lbr
    "00001011" -> Instruction2 Cmp
    "00001100" -> Instruction2 Prt
    "00001101" -> Instruction1 Rdc
    "11111110" -> Instruction0 TestHW
    "11111111" -> Instruction0 Stp

instructionToBitcode :: Instruction -> [String]
instructionToBitcode str = map (padLeft '0' 8) $ case str of
    Chl       -> ["00000000"]
    (Ldc x y) -> ["00000001", asBits x, asBits y]
    (Nul x)   -> ["00000010", asBits x]
    (Ld x y)  -> ["00000011", asBits x, asBits y]
    Sgf       -> ["00000100"]
    (Jmp x)   -> ["00000101", asBits x]
    (Pls x y) -> ["00000110", asBits x, asBits y]
    (Mns x y) -> ["00000111", asBits x, asBits y]
    (Zbr x)   -> ["00001000", asBits x]
    (Bbr x)   -> ["00001001", asBits x]
    (Lbr x)   -> ["00001010", asBits x]
    (Cmp x y) -> ["00001011", asBits x, asBits y]
    (Prt x y) -> ["00001100", asBits x, asBits y]
    (Rdc x)   -> ["00001101", asBits x]
    TestHW    -> ["11111110"]
    Stp       -> ["11111111"]

asBits :: Int -> String
asBits x = showIntAtBase 2 intToDigit x ""

padLeft :: Char -> Int -> String -> String
padLeft padder len str = replicate (len - length str) padder ++ str

byteToInstruction :: Byte -> InstructionT
byteToInstruction byte = bitcodeToInstruction $ show byte

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One = 1

byteToInt :: Byte -> Int
byteToInt (Byte ( b8, b7, b6, b5
                , b4, b3, b2, b1 )) = res
    where multipliers = map (2 ^) [7,6..0]
          bits = map bitToInt [b8, b7, b6, b5, b4, b3, b2, b1]
          res = sum $ zipWith (*) multipliers bits

parseOne :: [Byte] -> (Instruction, [Byte])
parseOne (b:bs) = case instT of
    Instruction0 i -> (i, bs)
    Instruction1 i1 -> (i1 $ head bsInt, tail bs)
    Instruction2 i2 -> let a1:a2:_ = bsInt in (i2 a1 a2, drop 2 bs)
    where instT = byteToInstruction b
          bsInt = map byteToInt bs
          bInt = byteToInt b

compile :: [Byte] -> Program
compile [] = []
compile bytes = parsedOne : compile others
    where (parsedOne, others) = parseOne bytes

