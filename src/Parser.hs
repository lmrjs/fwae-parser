module Parser
    (parse, execParse, FAE(..), OpType(..), Value(..), Env(..)) where

import Data.Functor (($>))
import Control.Applicative ((<*))
import Control.Monad (unless)
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
data Value  = NumV Double | ClosureV String FAE Env deriving (Show, Eq)
data Env    = MtEnv | AnEnv String Value Env deriving (Show, Eq)


-- Parser --
lexer = P.makeTokenParser emptyDef {
    P.identStart    = letter,         -- for convenience, enforce that identifiers must begin with a letter [a-zA-Z] (TODO: maybe figure out how to prevent numbers from being identifiers)
    P.identLetter   = alphaNum <|> oneOf "_-*",
    P.reservedNames = ["with", "fun", "if0"]
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

parser :: Parser u FAE
parser = return (Id "TODO: combine above parsers")

execParse :: String -> Either ParseError FAE
execParse = PC.parse (parser <* eof) ""


-- Interpreter --
lookupId :: String -> Env -> Value
lookupId id MtEnv = error $ "Unbound identifier " ++ id
lookupId id (AnEnv name value rest) =
    if   name == id
    then value
    else lookupId id rest

execOp :: OpType -> Value -> Value -> Value
execOp Add (NumV n1) (NumV n2) = NumV $ n1 + n2
execOp Sub (NumV n1) (NumV n2) = NumV $ n1 - n2
execOp Mul (NumV n1) (NumV n2) = NumV $ n1 * n2

-- TODO: allow explicit errors by changing return value to either Just Value or Either String Value
interp :: FAE -> Value
interp fae = helper fae MtEnv
    where helper :: FAE -> Env -> Value
          helper (Number n)          _   = NumV n
          helper (Op op lhs rhs)     env = execOp op (helper lhs env) (helper rhs env)
          helper (Id id)             env = lookupId id env
          helper (Fun param body)    env = ClosureV param body env
          helper (If0 cond on0 non0) env =
            let NumV n = helper cond env
            in  helper (if n == 0 then on0 else non0) env
          helper (App fun arg) env =
            let ClosureV param body cenv = helper fun env
                argVal = helper arg env
            in  helper body (AnEnv param argVal cenv)


-- Exported Functions --
parse :: IO ()
parse = do
    putStrLn "Enter FWAE code on a single line. Use :q to exit."
    let doParse input =
            case execParse input of
                Left  e -> putStrLn "Error parsing input:" >> print e
                Right r -> print r
        inputLoop = do
            input <- getLine
            if   (input == ":q")
            then putStrLn "Exiting..."
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
