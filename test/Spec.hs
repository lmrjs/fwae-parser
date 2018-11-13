import Parser
import Interpreter
import Test.Hspec
import Test.Hspec.Expectations (Expectation)

isParseError :: Either parseError FAE -> Bool
isParseError (Left _) = True
isParseError _ = False

isInterpError :: Either String CallTree -> Bool
isInterpError (Left _) = True
isInterpError _ = False

(<=>) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(<=>) = shouldBe
(<~>) :: (HasCallStack, Show a) => a -> (a -> Bool) -> Expectation
(<~>) = shouldSatisfy

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        describe "Identifier" $ do
            it "valid" $ do
                parse "x" <=> Right (Id "x")
            it "invalid" $ do
                parse "fun"  <~> isParseError
                parse "if0"  <~> isParseError
                parse "with" <~> isParseError
        describe "Number" $ do
            it "positive integer" $ do
                parse "10" <=> Right (Number 10)
            it "negative integer" $ do
                parse "-10" <=> Right (Number (-10))
            it "positive float" $ do
                parse "1.2" <=> Right (Number 1.2)
            it "negative float" $ do
                parse "-1.2" <=> Right (Number (-1.2))
            it "zero float" $ do
                parse "0.0" <=> Right (Number 0)
            it "error on malformed float" $ do
                parse ".01" <~> isParseError
        describe "Operator" $ do
            it "addition" $ do
                parse "{+ 1 2}" <=> Right (Op Add (Number 1) (Number 2))
            it "subtraction" $ do
                parse "{- 1 2}" <=> Right (Op Sub (Number 1) (Number 2))
            it "multiplication" $ do
                parse "{* 1 2}" <=> Right (Op Mul (Number 1) (Number 2))
        describe "If0" $ do
            it "no subexpressions" $ do
                parse "{if0 0 1 2}" <=> Right (If0 (Number 0) (Number 1) (Number 2))
            it "with subexpressions" $ do
                parse "{if0 {+ 1 2} {- 3 4} {* 5 6}}"
                    <=> Right (If0 (Op Add (Number 1) (Number 2))
                                          (Op Sub (Number 3) (Number 4))
                                          (Op Mul (Number 5) (Number 6)))
        describe "Function" $ do
            it "identifier body" $ do
                parse "{fun {x} x}" <=> Right (Fun "x" (Id "x"))
            it "number body" $ do
                parse "{fun {x} 1.2}" <=> Right (Fun "x" (Number 1.2))
            it "expression body" $ do
                parse "{fun {x} {+ x 1}}"
                    <=> Right (Fun "x" (Op Add (Id "x") (Number 1)))
            it "empty params" $ do
                parse "{fun {} 1}" <~> isParseError
            it "too many params" $ do
                parse "{fun {x y} 1}" <~> isParseError
        describe "Application" $ do
            it "apply number to number" $ do
                parse "{0 1}" <=> Right (App (Number 0) (Number 1))
            it "apply function to number" $ do
                parse "{{fun {x} x} 2}" <=> Right (App (Fun "x" (Id "x")) (Number 2))
            it "too few arguments" $ do
                parse "{{fun {x} x}}" <~> isParseError
            it "too many arguments" $ do
                parse "{{fun {x} x} 1 2}" <~> isParseError
        describe "With" $ do
            it "named number" $ do
                parse "{with {x 1} x}" <=> Right (App (Fun "x" (Id "x")) (Number 1))
            it "named function" $ do
                parse "{with {f {fun {x} x}} f}"
                    <=> Right (App (Fun "f" (Id "f")) (Fun "x" (Id "x")))
            it "empty with" $ do
                parse "{with {} x}" <~> isParseError
            it "too many expressions" $ do
                parse "{with {x 1 2} x}" <~> isParseError
        describe "Other" $ do
            it "empty braces" $ do
                parse "{}" <~> isParseError
            it "missing braces (+)" $ do
                parse "+ 1 2" <~> isParseError
            it "missing braces (-)" $ do
                parse "- 1 2" <~> isParseError
            it "missing braces (*)" $ do
                parse "* 1 2" <~> isParseError
            it "missing braces (just numbers)" $ do
                parse "0 1 2" <~> isParseError
            it "missing braces (just identifiers)" $ do
                parse "a b c" <~> isParseError
            it "mismatched braces" $ do
                parse "{{fun {x} x 1}" <~> isParseError
    describe "Interpreter" $ do
        describe "Identifier" $ do
            it "unbound identifier" $ do
                runTree "x" <~> isInterpError
        describe "Number" $ do
            it "positive float" $ do
                runTree "1.3" <=> Right (NumberTree (NumV 1.3, MtEnv))
            it "negative float" $ do
                runTree "-2.4" <=> Right (NumberTree (NumV (-2.4), MtEnv))
            it "zero float" $ do
                runTree "0" <=> Right (NumberTree (NumV 0, MtEnv))
        describe "Operator" $ do
            it "addition" $ do
                runTree "{+ 1 2}"
                    <=> Right (OpTree (NumV 3, MtEnv) 
                                             (NumberTree (NumV 1, MtEnv)) 
                                             (NumberTree (NumV 2, MtEnv)))
            it "subtraction" $ do
                runTree "{- 4 5}"
                    <=> Right (OpTree (NumV (-1), MtEnv)
                                             (NumberTree (NumV 4, MtEnv)) 
                                             (NumberTree (NumV 5, MtEnv)))
            it "multiplication" $ do
                runTree "{* 6 7}"
                    <=> Right (OpTree (NumV 42, MtEnv)
                                             (NumberTree (NumV 6, MtEnv))
                                             (NumberTree (NumV 7, MtEnv)))
            it "nested operations" $ do
                runTree "{* {+ 1 2} {- 4 5}}"
                    <=> Right (OpTree (NumV (-3), MtEnv)
                                             (OpTree (NumV 3, MtEnv)
                                                     (NumberTree (NumV 1, MtEnv))
                                                     (NumberTree (NumV 2, MtEnv)))
                                             (OpTree (NumV (-1), MtEnv)
                                                     (NumberTree (NumV 4, MtEnv))
                                                     (NumberTree (NumV 5, MtEnv))))
        describe "If0" $ do
            it "true condition" $ do
                runTree "{if0 0 1 10}"
                    <=> Right (If0Tree (NumV 1, MtEnv)
                                              (NumberTree (NumV 0, MtEnv))   -- cond
                                              (NumberTree (NumV 1, MtEnv)))  -- on0
            it "false condition" $ do
                runTree "{if0 1 1 10}" 
                    <=> Right (If0Tree (NumV 10, MtEnv)
                                              (NumberTree (NumV 1, MtEnv))   -- cond
                                              (NumberTree (NumV 10, MtEnv))) -- non0
            it "error on non-number condition" $ do
                runTree "{with {f {fun {x} x}} {if0 x 1 10}}" <~> isInterpError
        describe "Function" $ do
            it "simple function" $ do
                runTree "{fun {x} 0}"
                    <=> Right (FunTree (ClosureV "x" (Number 0) MtEnv, MtEnv))
        describe "Application" $ do
            it "simple function application" $ do
                runTree "{{fun {x} 0} 10}"
                    <=> Right (AppTree (NumV 0, MtEnv)
                                              (FunTree (ClosureV "x" (Number 0) MtEnv, MtEnv))
                                              (NumberTree (NumV 10, MtEnv))
                                              (NumberTree (NumV 0, (AnEnv "x" (NumV 10) MtEnv))))
            it "error applying non-closure" $ do
                runTree "{0 0}" <~> isInterpError
        describe "Other" $ do
            it "complex nested expression" $ do
                runTree "{with {f {fun {x} {* 2 x}}} {+ 10 {f 5}}}" <=>
                -- This desugars into:
                {-
                   {{fun {f} 
                      {+ 10 
                         {f 5}}}
                    {fun {x} 
                      {* 2 x}}}
                -}
                -- And so its interp tree should resemble:
                {-
                    AppTree
                        FunTree (f)
                        FunTree (x)
                        OpTree (+ 10 (f 5)) (body of function applied)
                            NumTree (10)
                            AppTree (f 5)
                                IdTree (f)
                                NumberTree (5)
                                OpTree (* 2 x) (body of function applied)
                                    NumTree (2)
                                    IdTree (x)
                -}
                    Right (AppTree (NumV 20, MtEnv)
                                   (FunTree (ClosureV "f" (Op Add (Number 10) (App (Id "f") (Number 5))) MtEnv, MtEnv))
                                   (FunTree (ClosureV "x" (Op Mul (Number 2) (Id "x")) MtEnv, MtEnv))
                                   (OpTree (NumV 20, AnEnv "f" (ClosureV "x" (Op Mul (Number 2) (Id "x")) MtEnv) MtEnv)
                                           (NumberTree (NumV 10, AnEnv "f" (ClosureV "x" (Op Mul (Number 2) (Id "x")) MtEnv) MtEnv))
                                           (AppTree (NumV 10, AnEnv "f" (ClosureV "x" (Op Mul (Number 2) (Id "x")) MtEnv) MtEnv)
                                                    (IdTree (ClosureV "x" (Op Mul (Number 2) (Id "x")) MtEnv, AnEnv "f" (ClosureV "x" (Op Mul (Number 2) (Id "x")) MtEnv) MtEnv))
                                                    (NumberTree (NumV 5, AnEnv "f" (ClosureV "x" (Op Mul (Number 2) (Id "x")) MtEnv) MtEnv))
                                                    (OpTree (NumV 10, AnEnv "x" (NumV 5) MtEnv)
                                                        (NumberTree (NumV 2, AnEnv "x" (NumV 5) MtEnv))
                                                        (IdTree (NumV 5, AnEnv "x" (NumV 5) MtEnv))))))
            