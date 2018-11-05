import Parser
import Test.Hspec

isParseError :: Either parseError FAE -> Bool
isParseError (Left _) = True
isParseError _ = False

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        describe "Identifier" $ do
            it "valid" $ do
                execParse "x" `shouldBe` Right (Id "x")
            it "invalid" $ do
                execParse "fun"  `shouldSatisfy` isParseError
                execParse "if0"  `shouldSatisfy` isParseError
                execParse "with" `shouldSatisfy` isParseError
        describe "Number" $ do
            it "positive integer" $ do
                execParse "10" `shouldBe` Right (Number 10)
            it "negative integer" $ do
                execParse "-10" `shouldBe` Right (Number (-10))
            it "positive float" $ do
                execParse "1.2" `shouldBe` Right (Number 1.2)
            it "negative float" $ do
                execParse "-1.2" `shouldBe` Right (Number (-1.2))
            it "zero float" $ do
                execParse "0.0" `shouldBe` Right (Number 0)
            it "error on malformed float" $ do
                execParse ".01" `shouldSatisfy` isParseError
        describe "Operator" $ do
            it "addition" $ do
                execParse "{+ 1 2}" `shouldBe` Right (Op Add (Number 1) (Number 2))
            it "subtraction" $ do
                execParse "{- 1 2}" `shouldBe` Right (Op Sub (Number 1) (Number 2))
            it "multiplication" $ do
                execParse "{* 1 2}" `shouldBe` Right (Op Mul (Number 1) (Number 2))
        describe "If0" $ do
            it "no subexpressions" $ do
                execParse "{if0 0 1 2}" `shouldBe` Right (If0 (Number 0) (Number 1) (Number 2))
            it "with subexpressions" $ do
                execParse "{if0 {+ 1 2} {- 3 4} {* 5 6}}"
                    `shouldBe` Right (If0 (Op Add (Number 1) (Number 2))
                                          (Op Sub (Number 3) (Number 4))
                                          (Op Mul (Number 5) (Number 6)))
        describe "Fun" $ do
            it "identifier body" $ do
                execParse "{fun {x} x}" `shouldBe` Right (Fun "x" (Id "x"))
            it "number body" $ do
                execParse "{fun {x} 1.2}" `shouldBe` Right (Fun "x" (Number 1.2))
            it "expression body" $ do
                execParse "{fun {x} {+ x 1}}"
                    `shouldBe` Right (Fun "x" (Op Add (Id "x") (Number 1)))
            it "empty params" $ do
                execParse "{fun {} 1}" `shouldSatisfy` isParseError
            it "too many params" $ do
                execParse "{fun {x y} 1}" `shouldSatisfy` isParseError
        describe "App" $ do
            it "apply number to number" $ do
                execParse "{0 1}" `shouldBe` Right (App (Number 0) (Number 1))
            it "apply function to number" $ do
                execParse "{{fun {x} x} 2}" `shouldBe` Right (App (Fun "x" (Id "x")) (Number 2))
            it "too few arguments" $ do
                execParse "{{fun {x} x}}" `shouldSatisfy` isParseError
            it "too many arguments" $ do
                execParse "{{fun {x} x} 1 2}" `shouldSatisfy` isParseError
        describe "With" $ do
            it "named number" $ do
                execParse "{with {x 1} x}" `shouldBe` Right (App (Fun "x" (Id "x")) (Number 1))
            it "named function" $ do
                execParse "{with {f {fun {x} x}} f}"
                    `shouldBe` Right (App (Fun "f" (Id "f")) (Fun "x" (Id "x")))
            it "empty with" $ do
                execParse "{with {} x}" `shouldSatisfy` isParseError
            it "too many expressions" $ do
                execParse "{with {x 1 2} x}" `shouldSatisfy` isParseError
        describe "Other" $ do
            it "empty braces" $ do
                execParse "{}" `shouldSatisfy` isParseError
            it "missing braces (+)" $ do
                execParse "+ 1 2" `shouldSatisfy` isParseError
            it "missing braces (-)" $ do
                execParse "- 1 2" `shouldSatisfy` isParseError
            it "missing braces (*)" $ do
                execParse "* 1 2" `shouldSatisfy` isParseError
            it "missing braces (just numbers)" $ do
                execParse "0 1 2" `shouldSatisfy` isParseError
            it "missing braces (just identifiers)" $ do
                execParse "a b c" `shouldSatisfy` isParseError
            it "mismatched braces" $ do
                execParse "{{fun {x} x 1}" `shouldSatisfy` isParseError
    describe "Interpreter" $ do
        it "TODO" $ do
            True `shouldBe` True
            