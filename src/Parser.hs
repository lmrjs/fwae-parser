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
data FAE = Number Double
         | Op     OpType FAE FAE
         | Id     String
         | If0    FAE FAE FAE
         | Fun    String FAE
         | App    FAE FAE
    deriving Show
data OpType = Add | Sub | Mul deriving Show
data Value  = NumV Double | ClosureV String FAE Env deriving Show
data Env    = MtEnv | AnEnv String Value Env deriving Show


-- Parser --
lexer = P.makeTokenParser emptyDef {
    identStart    = letter,         -- for convenience, enforce that identifiers must begin with a letter [a-zA-Z] (TODO: maybe figure out how to prevent numbers from being identifiers)
    identLetter   = alphaNum <|> oneOf "_-*",
    reservedNames = ["with", "fun", "if0", "+", "-", "*"]
}

-- a curated selection of parsers from http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Token.html#t:GenTokenParser
type Parser u a = Parsec String u a -- TODO: inputs String, outputs a -- so what does the `u` mean??
identifier = P.identifier lexer :: Parser u String          -- begins with identStart, contains identLetter, is not reservedNames
reserved   = P.reserved   lexer :: String -> Parser u ()    -- is one of reservedSymbols
number     = P.float      lexer :: Parser u Double          -- returns a Left Integer or a Right Double
whitespace = P.whiteSpace lexer :: Parser u ()              -- skips over whitespace
braces     = P.braces     lexer :: Parser u a -> Parser u a -- is enclosed in curly braces {...}

-- actual parser
parser = whitespace -- TODO: combine parsers above (using parser combinators!) to parse FWAE


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
