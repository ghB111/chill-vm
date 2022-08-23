{-# LANGUAGE NamedFieldPuns #-}
module Vm ( run
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
                        , TestHW
                        , Stp )
          , Program
          , ChillVm (ChillVm)) where
import System.IO.Unsafe (unsafePerformIO)

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

step :: ChillVm -> Instruction -> ChillVm
step vm Stp  = vm { state = Stopped }
step vm Chl = vm
step vm@ChillVm{registers} (Ldc reg const) = vm { registers = replaceNth reg const registers }
step vm (Nul reg) = vm { registers = replaceNth reg 0 $ registers vm }
step vm@ChillVm{registers} (Ld regDst regSrc) = vm { registers = replaceNth regDst copiedValue registers }
    where copiedValue = (registers !! regSrc)
step vm Sgf = error "This should be a segfault"
step vm@ChillVm{pc} Jmp{dst} = vm {pc = dst}
-- todo update ccr
step vm@ChillVm{registers} Pls{reg, regDst} = 
    let fst = registers !! reg
        snd = registers !! regDst
        newVal = fst + snd
        updatedRegs = replaceNth regDst newVal registers
    in step vm {registers = updatedRegs} $ Cmp reg regDst
step vm@ChillVm{registers} Mns{reg, regDst} =
    let fst = registers !! reg
        snd = registers !! regDst
        newVal = fst - snd
        updatedRegs = replaceNth regDst newVal registers
    in step vm {registers = updatedRegs} $ Cmp reg regDst
step vm@ChillVm{ccr = CCR{zero}} Zbr{dst}
    | zero = vm {pc = dst}
    | otherwise = vm
step vm@ChillVm{ccr = CCR{zero, sign}} Bbr{dst}
    | zero = vm
    | sign = vm
    | otherwise = vm {pc = dst}
step vm@ChillVm{ccr = CCR{zero, sign}} Lbr{dst}
    | zero = vm
    | sign = vm {pc = dst}
    | otherwise = vm
step vm@ChillVm{ccr = CCR{zero, sign}, registers} Cmp{reg1, reg2} =
    let reg1Val = registers !! reg1
        reg2Val = registers !! reg2
        s = signum (reg1Val - reg2Val)
        zero = s == 0
        sign = s < 0
        in vm{ccr = CCR{zero = zero, sign = sign}}
step vm TestHW = unsafePerformIO $ do
    putStrLn "Hello world!"
    return vm

performInstruction :: ChillVm -> Instruction -> ChillVm
performInstruction vm@ChillVm{state = Stopped} _ = vm
performInstruction vm@ChillVm{pc} instruction = (step vm { pc = succ pc } instruction) 
 
run :: Program -> ChillVm
run program = run' makeVm program
    where run' vm@ChillVm{state = Stopped} _ = vm
          run' vm@ChillVm{pc} program = let selectedInst = program !! pc
            in run' (performInstruction vm selectedInst) program

debug :: Program -> ChillVm
debug program = run' makeVm program
    where run' vm@ChillVm{state = Stopped} _ = unsafePerformIO $ do
              putStrLn "STOPPED"
              return vm
          run' vm@ChillVm{pc} program = let selectedInst = program !! pc
            in unsafePerformIO $ do
                print vm
                return $ run' (performInstruction vm selectedInst) program


exampleProgram = [Chl, Ldc 10 20, Ld 0 10, Stp]
exampleHwProgram = [TestHW, Jmp 0, Sgf]
exampleLoop =
    [ Ldc 0 10, Ldc 1 1
    , Chl
    , Cmp 0 255, Zbr 10
    , TestHW
    , Ldc 1 1, Mns 0 1, Ld 0 1, Jmp 1
    , Stp ] -- 10

