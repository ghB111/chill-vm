module Compiler where

import Vm
import Parser

compile :: [Byte] -> Program
compile = undefined

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
    "11111111" -> Instruction0 Stp

byteToInstruction :: Byte -> InstructionT
byteToInstruction byte = bitcodeToInstruction $ show byte

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One = 1

byteToInt :: Byte -> Int
byteToInt (Byte ( b8, b7, b6, b5
                , b4, b3, b2, b1 )) = res
    where multipliers = map (\x -> 2 ^ x) [7,6..0]
          bits = map bitToInt [b8, b7, b6, b5, b4, b3, b2, b1]
          res = sum $ map (\(x, y) -> x * y) $ zip multipliers bits

parseOne :: [Byte] -> (Instruction, [Byte])
parseOne (b:bs) = case instT of
    Instruction0 i -> (i, bs)
    Instruction1 i1 -> (i1 $ head bsInt, tail bs)
    Instruction2 i2 -> let a1:a2:_ = bsInt in (i2 a1 a2, drop 2 bs)
    where instT = byteToInstruction b
          bsInt = map byteToInt bs
          bInt = byteToInt b

