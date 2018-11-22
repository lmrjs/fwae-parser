module Interpreter
    (runInterp, interp, interpTree, run, runTree, callTreeToTree, CallTree(..), Value(..), Env(..), Metadata) where

import Parser
import Text.Groom (groom)
import Data.Tree (Tree(Node))


-- Call Tree --
data CallTree = NumberTree Metadata
              | IdTree     Metadata
              | OpTree     Metadata CallTree CallTree          -- lhs rhs
              | If0Tree    Metadata CallTree CallTree          -- cond on0/non0
              | FunTree    Metadata
              | AppTree    Metadata CallTree CallTree CallTree -- fun arg app
    deriving (Show, Eq)
data Value  = NumV Double | ClosureV String FAE Env deriving (Show, Eq)
data Env    = MtEnv | AnEnv String Value Env deriving (Show, Eq)
type Metadata = (Value, Env)

-- convert a CallTree to a Tree for pretty-printing
callTreeToTree :: CallTree -> Tree (String, Value, Env)
callTreeToTree (NumberTree (val, env))             = Node ("Number", val, env) []
callTreeToTree (IdTree     (val, env))             = Node ("Id",     val, env) []
callTreeToTree (OpTree     (val, env) lhs rhs)     = Node ("Op",     val, env) $ map callTreeToTree [lhs, rhs]
callTreeToTree (If0Tree    (val, env) cond run)    = Node ("If0",    val, env) $ map callTreeToTree [cond, run]
callTreeToTree (FunTree    (val, env))             = Node ("Fun",    val, env) []
callTreeToTree (AppTree    (val, env) fun arg app) = Node ("App",    val, env) $ map callTreeToTree [fun, arg, app]


-- Helpers --
getValue :: CallTree -> Value
getValue (NumberTree (value, _))       = value
getValue (IdTree     (value, _))       = value
getValue (OpTree     (value, _) _ _)   = value
getValue (If0Tree    (value, _) _ _)   = value
getValue (FunTree    (value, _))       = value
getValue (AppTree    (value, _) _ _ _) = value

isNumV :: String -> Value -> Either String Value
isNumV _   num@(NumV _) = Right num
isNumV msg errorVal     = Left $ msg ++ groom errorVal

isClosureV :: String -> Value -> Either String Value
isClosureV _   closure@(ClosureV _ _ _) = Right closure
isClosureV msg errorVal                 = Left $ msg ++ groom errorVal

lookupId :: String -> Env -> Either String Value
lookupId id MtEnv = Left $ "Unbound identifier " ++ id
lookupId id (AnEnv name value rest) =
    if   name == id
    then Right value
    else lookupId id rest

execOp :: OpType -> Value -> Value -> Value
execOp Add (NumV n1) (NumV n2) = NumV $ n1 + n2
execOp Sub (NumV n1) (NumV n2) = NumV $ n1 - n2
execOp Mul (NumV n1) (NumV n2) = NumV $ n1 * n2

if0NonNumErrorMsg     = "Interp error: attempted to use following non-number in the condition of an If0:\n"
appNonClosureErrorMsg = "Interp error: attempted to apply following non-closure as a function:\n"


-- Interpreter --
interp :: FAE -> Either String Value
interp fae = helper fae MtEnv
    where helper :: FAE -> Env -> Either String Value
          helper (Number n)          _   = Right $ NumV n
          helper (Op op lhs rhs)     env = do
            leftVal  <- helper lhs env
            rightVal <- helper rhs env
            Right $ execOp op leftVal rightVal
          helper (Id id)             env = lookupId id env
          helper (Fun param body)    env = Right $ ClosureV param body env
          helper (If0 cond on0 non0) env = do
            condVal <- helper cond env
            NumV n  <- isNumV if0NonNumErrorMsg condVal
            helper (if n == 0 then on0 else non0) env
          helper (App fun arg) env = do
            funVal <- helper fun env
            argVal <- helper arg env
            ClosureV param body cenv <- isClosureV appNonClosureErrorMsg funVal
            helper body (AnEnv param argVal cenv)

interpTree :: FAE -> Either String CallTree
interpTree fae = helper fae MtEnv
    where dummyTree = IdTree (NumV 0, MtEnv) -- TODO: implement interp producing call tree
          helper :: FAE -> Env -> Either String CallTree
          helper (Number n)          env = Right $ NumberTree (NumV n, env)
          helper (Op op lhs rhs)     env = do 
            n <- interp fae
            lTree <- interpTree lhs env
            rTree <- interpTree rhs env
            Right $ OpTree (n, env) lTree rTree
          helper (Id id)             env = do
            n <- lookupId id env
            Right $ IdTree (n, env)
          helper (Fun param body)    env = do
            n <- interp fae 
            Right $ FunTree (n, env)
          helper (If0 cond on0 non0) env = do    -- cond might be string
            cond <- interp cond env
            condTree <- interpTree cond env
            if cond == 0 
            then 
              n <- interp on0 env
              on0Tree <- interpTree on0 env
              Right $ If0Tree (n, env) condTree on0Tree
            else 
              n <- interp non0 env
              non0Tree <- interpTree non0 env
              Right $ If0Tree (n, env) condTree non0Tree
          helper (App fun arg)       env = 
              n <- interp fae 
              argTree <- interpTree arg env
              ClosureV param body cenv <- interp fun 
              funTree <- interpTree fun env
              app <- NumberTree (NumV n, (AnEnv param (getValue argTree) cenv))
            Right $ AppTree (n, env) argTree funTree app


-- Run from direct FWAE code
run :: String -> Either String Value
run input = parse input >>= interp

runTree :: String -> Either String CallTree
runTree input = parse input >>= interpTree

runInterp :: IO ()
runInterp = do
    let doParse input =
            case runTree input of
                Left  e    -> putStrLn e
                Right tree -> putStrLn . groom $ tree
        inputLoop = do
            putStrLn "Enter FWAE code on a single line. Use :c to close."
            input <- getLine
            if   (input == ":c")
            then putStrLn "Closing interpreter..."
            else doParse input >> inputLoop
    inputLoop
