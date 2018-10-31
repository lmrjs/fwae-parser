{-# LANGUAGE GADTs #-}

module Parser
    ( parser
    ) where

import qualified Text.Parsec as Parsec

{-
    EBNF:
    <FWAE> :: <num>
            | {<op> <FWAE> <FWAE>}
            | {with {<id> <FWAE>} <FWAE>}
            | <id>
            | {fun {<id>} <FWAE>}
            | {<FWAE> <FWAE>}
            | {if0 <FWAE> <FWAE> <FWAE>}
    <op>   :: + | - | *
    <id>   :: (* begins with letter, is alphanumeric or `-` or `_` or `*` *)
              (* is none of `with`, `fun`, `+`, `-`, `*` *)
-}

data FAE where
    Number :: (Num a, Show a) => {
        num :: a
    } -> FAE

parser :: IO ()
parser = putStrLn "TODO"
