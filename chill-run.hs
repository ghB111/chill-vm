import Prelude hiding (fromString)
import System.Environment
import Data.List  
import Control.DeepSeq (deepseq, NFData(rnf))

import Parser (parse, fromString)
import Compiler (compile)
import qualified Vm (run, run_, ChillVm)

runVerbose :: String -> IO ()
runVerbose sourceName = do
    putStrLn "Running in verbose mode"
    input <- readFile sourceName
    putStrLn $ "Before processing:\n" ++ input
    let parsed = parse input
    putStrLn $ "After parsing:\n" ++ unlines parsed
    let bytecode = map fromString parsed
    let program = compile bytecode
    putStrLn $ "Program is:\n" ++ show program
    finalState <- Vm.run program
    putStrLn $ "Final state:\n" ++ show finalState

run :: String -> IO ()
run sourceName = do
    input <- readFile sourceName
    let parsed = parse input
    let bytecode = map fromString parsed
    let program = compile bytecode
    Vm.run_ program

showUsage = putStrLn "Usage: chill-run path-to-text"

main = do  
    args <- getArgs
    if head args == "-v"
        then if length args /= 2 then showUsage else runVerbose $ args !! 1
        else if length args == 1
            then run $ head args
            else showUsage
