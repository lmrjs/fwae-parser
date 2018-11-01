module Main where

import Parser
import Visualizer

main :: IO ()
main = parse >> visualize
