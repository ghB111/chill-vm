{-# LANGUAGE NamedFieldPuns #-}
module Vm ( run
          , run_
          , Instruction ( Chl
                        , Ldc
                        , Nul
                        , Ld
                        , Sgf
                        , Jmp
                        , Pls
                        , Mns
                        , Zbr
                        , Bbr
                        , Lbr
                        , Cmp
                        , Prt
                        , TestHW
                        , Stp )
          , Program
          , ChillVm (ChillVm)) where
import Data.Char (chr)

-- todo add 8bit number support

data State = Running | Stopped deriving (Show)

data CCR = CCR {
    zero :: Bool,
    sign :: Bool
} deriving (Show)

registersN = 256
data ChillVm = ChillVm {
    registers :: [Int], -- 256 registers
    pc :: Int, -- current instruction index, starting from 0.
               -- doesn't know instruction parameters, so equals line number
    ccr :: CCR,
    state :: State
} deriving (Show)

type Register = Int
type Address = Int

data Instruction = 
    Chl | 
    Ldc {reg :: Register, const :: Register} |
    Nul {reg :: Register} |
    Ld {regDst :: Register, regSrc :: Register} |
    Sgf |
    Jmp {dst :: Address} |
    Pls {reg :: Register, regDst :: Register} |
    Mns {reg :: Register, regDst :: Register} |
    Zbr {dst :: Address} |
    Bbr {dst :: Address} |
    Lbr {dst :: Address} |
    Cmp {reg1 :: Register, reg2 :: Register} |
    Prt {startReg :: Register, lenReg :: Register} |
    Stp |
    TestHW
    deriving (Show)

type Program = [Instruction]

makeVm = ChillVm {
    registers = replicate registersN 0,
    pc = 0,
    ccr = (CCR True False),
    state = Running
}

-- https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

step :: ChillVm -> Instruction -> IO (ChillVm)
step vm Stp  = return $ vm { state = Stopped }
step vm Chl = return vm
step vm@ChillVm{registers} (Ldc reg const) = return $ vm { registers = replaceNth reg const registers }
step vm (Nul reg) = return $ vm { registers = replaceNth reg 0 $ registers vm }
step vm@ChillVm{registers} (Ld regDst regSrc) = return $ vm { registers = replaceNth regDst copiedValue registers }
    where copiedValue = (registers !! regSrc)
step vm Sgf = return $ error "This should be a segfault"
step vm@ChillVm{pc} Jmp{dst} = return $ vm {pc = dst}
step vm@ChillVm{registers} Pls{reg, regDst} = 
    let fst = registers !! reg
        snd = registers !! regDst
        newVal = fst + snd
        updatedRegs = replaceNth regDst newVal registers
    in  step vm {registers = updatedRegs} $ Cmp reg regDst
step vm@ChillVm{registers} Mns{reg, regDst} =
    let fst = registers !! reg
        snd = registers !! regDst
        newVal = fst - snd
        updatedRegs = replaceNth regDst newVal registers
    in  step vm {registers = updatedRegs} $ Cmp reg regDst
step vm@ChillVm{ccr = CCR{zero}} Zbr{dst}
    | zero = return vm {pc = dst}
    | otherwise = return vm
step vm@ChillVm{ccr = CCR{zero, sign}} Bbr{dst}
    | zero = return vm
    | sign = return vm
    | otherwise = return vm {pc = dst}
step vm@ChillVm{ccr = CCR{zero, sign}} Lbr{dst}
    | zero = return vm
    | sign = return vm {pc = dst}
    | otherwise = return vm
step vm@ChillVm{registers} Prt{startReg, lenReg} =
    let start = registers !! startReg
        len = registers !! lenReg
        resStr = map chr $ slice start (start + len) registers
    in do
        putStr resStr
        return vm
step vm@ChillVm{ccr = CCR{zero, sign}, registers} Cmp{reg1, reg2} =
    let reg1Val = registers !! reg1
        reg2Val = registers !! reg2
        s = signum (reg1Val - reg2Val)
        zero = s == 0
        sign = s < 0
        in return vm{ccr = CCR{zero = zero, sign = sign}}
step vm TestHW = do
    putStrLn "Hello world!"
    return vm

performInstruction :: ChillVm -> Instruction -> IO (ChillVm)
performInstruction vm@ChillVm{state = Stopped} _ = return vm
performInstruction vm@ChillVm{pc} instruction = (step vm { pc = succ pc } instruction) 
 
run :: Program -> IO (ChillVm)
run program = run' makeVm program
    where run' :: ChillVm -> Program -> IO (ChillVm)
          run' vm@ChillVm{state = Stopped} _ = return vm
          run' vm@ChillVm{pc} program = let selectedInst = program !! pc
            in do
                newState <-performInstruction vm selectedInst
                run' newState program

run_ :: Program -> IO ()
run_ program = do
    run program
    return ()

debug :: Program -> IO (ChillVm)
debug program = run' makeVm program
    where run' :: ChillVm -> Program -> IO (ChillVm)
          run' vm@ChillVm{state = Stopped} _ = do
              putStrLn "STOPPED"
              return vm
          run' vm@ChillVm{pc} program = let selectedInst = program !! pc
            in do
                print vm
                newState <- performInstruction vm selectedInst
                run' newState program


exampleProgram = [Chl, Ldc 10 20, Ld 0 10, Stp]
exampleHwProgram = [TestHW, Jmp 0, Sgf]
exampleLoop =
    [ Ldc 0 10, Ldc 1 1
    , Chl
    , Cmp 0 255, Zbr 10
    , TestHW
    , Ldc 1 1, Mns 0 1, Ld 0 1, Jmp 1
    , Stp ] -- 10

exampleFairHelloWorld = 
    [ Ldc 200 72
    , Ldc 201 101
    , Ldc 202 108
    , Ldc 203 108
    , Ldc 204 111
    , Ldc 205 32
    , Ldc 206 87
    , Ldc 207 111
    , Ldc 208 114
    , Ldc 209 108
    , Ldc 210 100
    , Ldc 211 33
    , Ldc 212 10
    , Ldc 0 200
    , Ldc 1 13
    , Prt 0 1
    , Stp ]

