module Main where

import Parser
import Interpreter
import Visualizer

main :: IO ()
main = do
    let inputLoop = do
            putStrLn "Commands:\n  :parse to run the parser\n  :interp to run the interpreter\n  :viz to run the visualizer\n  :q to quit"
            input <- getLine
            case input of
                ":parse"  -> runParse  >> inputLoop
                ":interp" -> runInterp >> inputLoop
                ":viz"    -> visualize >> inputLoop
                ":q"      -> putStrLn "Quitting..."
                _         -> putStrLn "Command not found." >> inputLoop
    inputLoop
