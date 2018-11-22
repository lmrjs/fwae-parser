module Parser
    (runParse, parse, FAE(..), OpType(..)) where

import Data.Functor (($>))
import Control.Applicative ((<*))
import Control.Monad (unless)
import Text.Groom (groom)
import qualified Text.Parsec as PC (parse)
import Text.Parsec
    (Parsec, ParseError, choice, try, eof, (<|>))
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char
    (char, letter, alphaNum, oneOf)


-- EBNF for FWAE --
{-
    <FWAE> :: <num>
            | <id>
            | {<op> <FWAE> <FWAE>}
            | {if0 <FWAE> <FWAE> <FWAE>}
            | {fun {<id>} <FWAE>}
            | {<FWAE> <FWAE>}
            | {with {<id> <FWAE>} <FWAE>}
    <op>   :: + | - | *
    <id>   :: (* begins with letter, rest are alphanumeric or `-` or `_` or `*` *)
              (* is none of `with`, `fun`, `if0`, `+`, `-`, `*` *)
-}


-- AST --
data NumV = Integer
            | Double
            deriving (Show, Eq)
data FAE = Number Double
         | Id     String
         | Op     OpType FAE FAE
         | If0    FAE FAE FAE
         | Fun    String FAE
         | App    FAE FAE
    deriving (Show, Eq)
data OpType = Add | Sub | Mul deriving (Show, Eq)


-- Parser --
lexer = P.makeTokenParser emptyDef {
    P.identStart    = letter,         -- for convenience, enforce that identifiers must begin with a letter [a-zA-Z] (TODO: maybe figure out how to prevent numbers from being identifiers)
    P.identLetter   = alphaNum <|> oneOf "_-*",
    P.reservedNames = ["with", "fun", "if0", "+", "-", "*"]
}

-- a curated selection of parsers from http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Token.html#t:GenTokenParser
type Parser u a = Parsec String u a -- TODO: inputs String, outputs a -- so what does the `u` mean??
identifier = P.identifier lexer :: Parser u String          -- begins with identStart, contains identLetter, is not reservedNames
reserved   = P.reserved   lexer :: String -> Parser u ()    -- is one of reservedSymbols
integer    = P.integer    lexer :: Parser u Integer         -- returns an Integer (can be prefixed with - or +)
lexeme     = P.lexeme     lexer :: Parser u a -> Parser u a -- parses as a lexeme (strips whitespace from end)
whitespace = P.whiteSpace lexer :: Parser u ()              -- skips over whitespace
braces     = P.braces     lexer :: Parser u a -> Parser u a -- is enclosed in curly braces {...}
float = lexeme sign <*> P.float lexer :: Parser u Double    -- P.float does not parse sign!!

-- stolen from http://hackage.haskell.org/package/parsec-3.1.13.0/docs/src/Text.Parsec.Token.html#local-6989586621679063899
sign =   char '-' $> negate
     <|> char '+' $> id
     <|> return id

toOpType :: String -> OpType
toOpType "+" = Add
toOpType "-" = Sub
toOpType "*" = Mul

removeFrontWhiteSpace :: String -> String
if head x == ' '
    then removeFrontWhiteSpace tail x
    else x

-- actual parser
floatParser   = do
    n <- float
    return (Number n)
integerParser = do
    n <- integer
    return (Number (fromInteger n :: Double))
idParser      = do
    id <- identifier
    return (Id id)
opTypeParser  = do
    op <- identifier
    return (toOpType op)
opParser      = do { op <- braces
                ; op2 <- identifier
                --; l <- lexeme
                --; r <- lexeme
                --; let optype = parse op
                --; let l = parse l
                --; let r = parse r
                ; return (op2) }
if0Parser     = return (Id "TODO") :: Parser u FAE
funParser     = return (Id "TODO") :: Parser u FAE
appParser     = return (Id "TODO") :: Parser u FAE
withParser    = return (Id "TODO") :: Parser u FAE

parser :: Parser u FAE
parser = choice $
    map try [floatParser, integerParser, idParser, opParser, if0Parser, funParser, appParser, withParser]

parse :: String -> Either String FAE
parse input = case PC.parse (whitespace >> parser <* eof) "" input of
    Left  e   -> Left $ show e
    Right fae -> Right fae


-- Exported Functions --
runParse :: IO ()
runParse = do
    let doParse input =
            case parse input of
                Left  e   -> putStrLn $ "Error parsing input:\n" ++ e
                Right fae -> putStrLn . groom $ fae
        inputLoop = do
            putStrLn "Enter FWAE code on a single line. Use :c to close."
            input <- getLine
            if   (input == ":c")
            then putStrLn "Closing parser..."
            else doParse input >> inputLoop
    inputLoop


{-
    Important Haskell concepts used in this file so far:
        - modules, imports
        - do syntax, case-of syntax
        - data constructors, type synonyms
        - typeclasses, `deriving`
        - Either a b, Functor f, Applicative f, Monad m, IO a
-}
