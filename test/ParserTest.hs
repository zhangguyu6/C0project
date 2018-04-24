module ParserTest where 
import Test.Tasty.Hspec
import Frontend.Parser
import Frontend.Ast
import Data.Either(isLeft)
-- import Frontend.Token (LexemeCLass(..))



parserTests :: Spec
parserTests = describe "ParserTests" $ do 
    context "test expr" $ do
        it "test int" $
            parseExpFromStr "1" `shouldBe` Right (Number 1) 
        it "test add" $
            parseExpFromStr "1+1" `shouldBe` Right (GetBinop Add (Number 1) (Number 1))
        it "test sub" $
            parseExpFromStr "1-1" `shouldBe` Right (GetBinop Min (Number 1) (Number 1))
        it "test mul" $
            parseExpFromStr "1*1" `shouldBe` Right (GetBinop Mul (Number 1) (Number 1))
        it "test div" $
            parseExpFromStr "1/1" `shouldBe` Right (GetBinop Div (Number 1) (Number 1))
        it "test mod" $
            parseExpFromStr "1%1" `shouldBe` Right (GetBinop Mod (Number 1) (Number 1))
        it "test ident" $
            parseExpFromStr "a%1" `shouldBe` Right (GetBinop Mod (RIdent (CIdent "a")) (Number 1))
        it "test precedence" $
            parseExpFromStr "1+1*1"  `shouldBe` Right (GetBinop Add (Number 1) (GetBinop Mul (Number 1) (Number 1)))
        it "test negate" $
            parseExpFromStr "-1%1" `shouldBe` Right (GetBinop Mod (GetPrefixExp Unary(Number 1)) (Number 1))
        it "test parens" $
            parseExpFromStr "(-1)*(1+1)" `shouldBe` Right (GetBinop Mul (GetPrefixExp Unary(Number 1)) (GetBinop Add (Number 1) (Number 1)))
        it "test parens fail" $
            parseExpFromStr "(-1" `shouldSatisfy` isLeft 
        it "test op fail" $
            parseExpFromStr "*1" `shouldSatisfy` isLeft
    context "test lvalue" $ do 
        it "test ident as lvalue" $
            parseLvalueFromStr "a" `shouldBe` Right (LIdent (CIdent "a"))
        it "test (ident) as lvalue" $
            parseLvalueFromStr "(a)" `shouldBe` Right (LIdent (CIdent "a"))
    context "test simp" $ do 
        it "test ass" $ 
            parseSimpFromStr "a = 1" `shouldBe` Right (Sp (LIdent (CIdent "a")) Ass (Number 1))
        it "test ada" $
            parseSimpFromStr "a += 1" `shouldBe` Right (Sp (LIdent (CIdent "a")) Ass (GetBinop Add (RIdent (CIdent "a")) (Number 1)))
        it "test mia" $
            parseSimpFromStr "a -= 1" `shouldBe` Right (Sp (LIdent (CIdent "a")) Ass (GetBinop Min (RIdent (CIdent "a")) (Number 1)))
        it "test mua" $
            parseSimpFromStr "a *= 1" `shouldBe` Right (Sp (LIdent (CIdent "a")) Ass (GetBinop Mul (RIdent (CIdent "a")) (Number 1)))
        it "test moa" $
            parseSimpFromStr "a %= 1" `shouldBe` Right (Sp (LIdent (CIdent "a")) Ass (GetBinop Mod (RIdent (CIdent "a")) (Number 1)))
        it "test dia" $
            parseSimpFromStr "a /= 1" `shouldBe` Right (Sp (LIdent (CIdent "a")) Ass (GetBinop Div (RIdent (CIdent "a")) (Number 1)))
    context "test dec" $ do 
        it "test decOnly" $
            parseDeclFromStr "int a" `shouldBe` Right (Dec CInt (CIdent "a"))
        it "test decInit" $
            parseDeclFromStr "int a = 1" `shouldBe` Right (Init CInt (CIdent "a") (Number 1))
        it "test fail" $
            parseDeclFromStr "int a = 1 a + 1" `shouldSatisfy` isLeft
    context "test stml" $ do 
        it "test decl" $
            parseStmlFromStr "int a = 2;" `shouldBe` Right (GetDecl (Init CInt (CIdent "a") (Number 2)))
        it "test simp" $
            parseStmlFromStr "a = 2;" `shouldBe` Right (GetSimp (Sp (LIdent (CIdent "a")) Ass (Number 2)))
        it "test return" $
            parseStmlFromStr "return 1 + 1;" `shouldBe` Right (Return $ GetBinop Add (Number 1) (Number 1))
    context "test stmls" $ do 
        it "test single stml" $
            parseStmlsFromStr "int a = 1;" `shouldBe` Right (GetOneStml (GetDecl (Init CInt (CIdent "a") (Number 1))))
        it "test mul stmls" $
            parseStmlsFromStr "int a = 1;int b = 2;" `shouldBe` Right (MoreStmls (GetDecl (Init CInt (CIdent "a") (Number 1))) (GetOneStml (GetDecl (Init CInt (CIdent "b") (Number 2)))))
        it "test block" $
            parseStmlsFromStr "{int a = 1;int b = 2;}" `shouldBe` Right (GetBlock (GetStmls (MoreStmls (GetDecl (Init CInt (CIdent "a") (Number 1))) (GetOneStml (GetDecl (Init CInt (CIdent "b") (Number 2)))))))
    context "test block" $ 
        it "test block" $
            parseBlockFromStr "{int a = 1;}" `shouldBe` Right (GetStmls (GetOneStml (GetDecl (Init CInt (CIdent "a") (Number 1)))))
    context "test program" $
        it "test program" $
            parsePgFromStr "int main() { int a = 1; int b =2; a = b; return b;}" `shouldBe` Right (Pg (GetStmls (MoreStmls (GetDecl (Init CInt (CIdent "a") (Number 1))) 
            (MoreStmls (GetDecl (Init CInt (CIdent "b") (Number 2))) (MoreStmls (GetSimp (Sp(LIdent (CIdent "a")) Ass (RIdent (CIdent "b")))) (GetOneStml (Return (RIdent (CIdent "b")))))))))