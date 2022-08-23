import Prelude hiding (fromString)
import System.Environment
import Data.List  
import Control.DeepSeq (deepseq, NFData(rnf))

import Parser (parse, fromString)
import Compiler (compile)
import qualified Vm (run, ChillVm)

runVerbose :: String -> IO ()
runVerbose sourceName = do
    input <- readFile sourceName
    putStrLn $ "Before processing:\n" ++ input
    let parsed = parse input
    putStrLn $ "After parsing:\n" ++ (unlines parsed)
    let bytecode = map fromString parsed
    let program = compile bytecode
    putStrLn $ "Program is:\n" ++ show program
    let finalState = Vm.run program
    putStrLn $ "Final state:\n" ++ show finalState

instance NFData Vm.ChillVm where
    rnf a = a `seq` ()

run :: String -> IO ()
run sourceName = do
    input <- readFile sourceName
    let parsed = parse input
    let bytecode = map fromString parsed
    let program = compile bytecode
    let finalState = Vm.run program
    finalState `deepseq` (return ())

showUsage = do
    putStrLn "Usage: chill-run path-to-text"

main = do  
    args <- getArgs
    if head args == "-v"
        then do
            if length args /= 2
                then showUsage
                else run $ args !! 1
        else if length args == 1
            then run $ head args
            else showUsage

