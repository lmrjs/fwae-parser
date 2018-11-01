module Parser
( parse, FAE, OpType, Value, Env
) where

import Text.Parsec (Parsec)
import qualified Text.Parsec.Token as P       -- http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Token.html
import Text.ParserCombinators.Parsec          -- http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Combinator.html
        (letter, alphaNum, oneOf, (<|>))
import qualified Text.ParserCombinators.Parsec as PC (parse)
import Text.ParserCombinators.Parsec.Language -- http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Language.html
        (emptyDef, identStart, identLetter, reservedNames)


-- EBNF for FWAE --
{-
    <FWAE> :: <num>
            | {<op> <FWAE> <FWAE>}
            | {with {<id> <FWAE>} <FWAE>}
            | <id>
            | {fun {<id>} <FWAE>}
            | {<FWAE> <FWAE>}
            | {if0 <FWAE> <FWAE> <FWAE>}
    <op>   :: + | - | *
    <id>   :: (* begins with letter, rest are alphanumeric or `-` or `_` or `*` *)
              (* is none of `with`, `fun`, `if0`, `+`, `-`, `*` *)
              (* these need to be statically checked during parsing *)
-}


-- AST --
data FAE =
    Number { num :: Either Integer Double } |
    Op     { op :: OpType, lhs :: FAE, rhs :: FAE } |
    Id     { id :: String } |
    Fun    { param :: String, body :: FAE } |
    App    { fun :: FAE, arg :: FAE } |
    If0    { cond :: FAE, on0 :: FAE, non0 :: FAE }
    deriving Show
data OpType = Add | Sub | Mul deriving Show


-- Value and Environment --
-- not really needed unless we will implement interp
data Value =
    NumV     { n :: Either Integer Double } |
    ClosureV { fname :: String, fbody :: FAE, cenv :: Env }
data Env = MtEnv | AnEnv { name :: String, value :: Value, restEnv :: Env }

lookup :: String -> Env -> Value
lookup str env = NumV $ Left 0 -- TODO


-- Parser --
allowedSymbols = ['_', '-', '*']
reservedSymbols  = ["with", "fun", "if0", "+", "-", "*"]

lexer = P.makeTokenParser emptyDef {
    identStart    = letter,         -- for convenience, enforce that identifiers must begin with a letter [a-zA-Z] (TODO: maybe figure out how to prevent numbers from being identifiers)
    identLetter   = alphaNum <|> oneOf allowedSymbols,
    reservedNames = reservedSymbols
}

-- a curated selection of parsers from http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Token.html#t:GenTokenParser
type Parser u a = Parsec String u a -- TODO: inputs String, outputs a -- so what does the `u` mean??
identifier = P.identifier     lexer :: Parser u String                  -- begins with identStart, contains identLetter, is not reservedNames
reserved   = P.reserved       lexer :: String -> Parser u ()            -- is one of reservedSymbols
number     = P.naturalOrFloat lexer :: Parser u (Either Integer Double) -- returns a Left Integer or a Right Double
whitespace = P.whiteSpace     lexer :: Parser u ()                      -- skips over whitespace
braces     = P.braces         lexer :: Parser u a -> Parser u a         -- is enclosed in curly braces {...}

-- actual parser
parser = whitespace -- TODO: combine parsers above (using parser combinators!) to parse FWAE


-- Exported Functions --
parse :: IO ()
parse = do
    print "Enter FWAE code here. Use CTRL-C to exit."
    input <- getContents -- TODO: figure out how to get all contents instead of just the first line
    case PC.parse parser "" input of
        Left  e -> print $ "Error parsing input: \n" ++ show e
        Right r -> print r


{-
    Important Haskell concepts used in this file so far:
        - modules, imports
        - do syntax, case-of syntax
        - ($) function application, (.) function composition, <|> whatever that is
        - data constructors, record syntax, type synonyms
        - typeclasses, `deriving`
        - Either a b, Monad m or Applicative f (or neither, they're not that important for comprehension), IO a
-}
