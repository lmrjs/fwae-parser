module Visualizer
( visualize
) where

import Parser
import Interpreter
import Data.Tree.Pretty (drawVerticalTree)

visualize :: IO ()
visualize = do
    let doParse input =
            -- case runTree input of -- for interp tree
            case parse input of
                Left  e   -> putStrLn e
                Right fae -> putStrLn $ (drawVerticalTree . faeToTree) fae
        inputLoop = do
            putStrLn "Enter FWAE code on a single line. Use :c to close."
            input <- getLine
            if   (input == ":c")
            then putStrLn "Closing visualizer..."
            else doParse input >> inputLoop
    inputLoop