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
type Parser = Parsec String ()
identifier = P.identifier lexer :: Parser String          -- begins with identStart, contains identLetter, is not reservedNames
reserved   = P.reserved   lexer :: String -> Parser ()    -- is one of reservedSymbols
integer    = P.integer    lexer :: Parser Integer         -- returns an Integer (can be prefixed with - or +)
lexeme     = P.lexeme     lexer :: Parser a -> Parser a   -- parses as a lexeme (strips whitespace from end)
whitespace = P.whiteSpace lexer :: Parser ()              -- skips over whitespace
braces     = P.braces     lexer :: Parser a -> Parser a   -- is enclosed in curly braces {...}
float = lexeme sign <*> P.float lexer :: Parser Double    -- P.float does not parse sign!!

-- stolen from http://hackage.haskell.org/package/parsec-3.1.13.0/docs/src/Text.Parsec.Token.html#local-6989586621679063899
sign =   char '-' $> negate
     <|> char '+' $> id
     <|> return id

-- actual parser
floatParser   = return (Id "TODO") :: Parser u FAE
integerParser = return (Id "TODO") :: Parser u FAE
idParser      = return (Id "TODO") :: Parser u FAE
opTypeParser  = return (Id "TODO") :: Parser u FAE
opParser      = return (Id "TODO") :: Parser u FAE
if0Parser     = return (Id "TODO") :: Parser u FAE
funParser     = return (Id "TODO") :: Parser u FAE
appParser     = return (Id "TODO") :: Parser u FAE
withParser    = return (Id "TODO") :: Parser u FAE

parser :: Parser FAE
parser = choice $ map try
    [ floatParser
    , integerParser
    , idParser
    , opParser
    , if0Parser
    , funParser
    , appParser
    , withParser ]

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
