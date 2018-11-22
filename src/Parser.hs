module Parser
    (runParse, parse, faeToTree, FAE(..), OpType(..)) where

import Data.Tree (Tree(Node))
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

-- convert an FAE into a Tree for pretty-printing
faeToTree :: FAE -> Tree String
faeToTree (Number n)          = Node ("Number: "         ++ show n)      []
faeToTree (Id id)             = Node ("Id: "             ++ id)          []
faeToTree (Fun param body)    = Node ("Fun with param: " ++ param)       [faeToTree body]
faeToTree (Op optype lhs rhs) = Node ("Op: "             ++ show optype) $ map faeToTree [lhs, rhs]
faeToTree (If0 cond on0 non0) = Node "If0" $ map faeToTree [cond, on0, non0]
faeToTree (App fun arg)       = Node "App" $ map faeToTree [fun, arg]


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
floatParser = do
    n <- float
    return (Number n)
integerParser = do
    n <- integer
    return (Number (fromInteger n))
idParser = do
    id <- identifier
    return (Id id)
opTypeParser = do
        reserved "+"
        return Add
    <|> do
        reserved "-"
        return Sub
    <|> do
        reserved "*"
        return Mul
opParser = braces $ do
    opType <- opTypeParser
    lhs    <- parser
    rhs    <- parser
    return (Op opType lhs rhs)
if0Parser = braces $ do
    reserved "if0"
    cond <- parser
    on0  <- parser
    non0 <- parser
    return (If0 cond on0 non0)
funParser = braces $ do
    reserved "fun"
    param <- braces identifier
    body  <- parser
    return (Fun param body)
appParser = braces $ do
    fun <- parser
    arg <- parser
    return (App fun arg)
withParser = braces $ do
    reserved "with"
    (name, expr) <- braces (do
        name <- identifier
        expr <- parser
        return (name, expr))
    body <- parser
    return (App (Fun name body) expr)

-- parser implementation using operators
{-
floatParser   = Number <$> float
integerParser = Number . fromIntegral <$> integer
idParser      = Id <$> identifier
opTypeParser  = reserved "+" $> Add
            <|> reserved "-" $> Sub
            <|> reserved "*" $> Mul
opParser   = braces $ Op <$> opTypeParser <*> parser <*> parser
if0Parser  = braces $ reserved "if0" >> If0 <$> parser <*> parser <*> parser
funParser  = braces $ reserved "fun" >> Fun <$> braces identifier <*> parser
appParser  = braces $ App <$> parser <*> parser
withParser = braces $ do reserved "with"
                         (name, expr) <- braces $ (,) <$> identifier <*> parser
                         body <- parser
                         return $ App (Fun name body) expr
-}

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
