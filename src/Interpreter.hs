module Interpreter
    (runInterp, interp, interpTree, run, runTree, CallTree(..), Metadata) where

import Parser
import Text.Groom (groom)


-- Call Tree --
data CallTree = NumberTree Metadata
              | IdTree     Metadata
              | OpTree     Metadata CallTree CallTree          -- lhs rhs
              | If0Tree    Metadata CallTree CallTree          -- cond on0/non0
              | FunTree    Metadata
              | AppTree    Metadata CallTree CallTree CallTree -- fun arg app
    deriving (Show, Eq)
type Metadata = (Value, Env)


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
          helper (Id id)             env = Right $ lookupId id env
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
          helper (Number n)          env = Right $ dummyTree
          helper (Op op lhs rhs)     env = Right $ dummyTree
          helper (Id id)             env = Right $ dummyTree
          helper (Fun param body)    env = Right $ dummyTree
          helper (If0 cond on0 non0) env = Right $ dummyTree
          helper (App fun arg)       env = Right $ dummyTree


-- Run from direct FWAE code
run :: String -> Value
run input = case parse input >>= interp of
    Left  e   -> error $ show e
    Right fae -> fae

runTree :: String -> CallTree
runTree input = case parse input >>= interpTree of
    Left  e    -> error $ show e
    Right tree -> tree

runInterp :: IO ()
runInterp = do
    let doParse input =
            case parse input >>= interpTree of
                Left  e    -> putStrLn e
                Right tree -> putStrLn . groom $ tree
        inputLoop = do
            putStrLn "Enter FWAE code on a single line. Use :c to close."
            input <- getLine
            if   (input == ":c")
            then putStrLn "Closing interpreter..."
            else doParse input >> inputLoop
    inputLoop
