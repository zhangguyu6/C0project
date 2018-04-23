module Main (main) where 
import Test.Tasty
import Test.Tasty.Hspec
import Frontend.Lexer(scanner,CTok)
import Frontend.Token (LexemeCLass(..))
import Control.Monad(mapM)
import Data.Either(isLeft)
main :: IO ()
main = defaultMainWithSpec [lexerTests]

scannerToTok:: Either String [CTok] -> Either String [LexemeCLass]
scannerToTok (Left a) = Left a 
scannerToTok (Right a) = Right (fmap fst a) 

sTok :: String -> Either String [LexemeCLass]
sTok = scannerToTok . scanner 

defaultMainWithSpec :: [Spec] -> IO ()
defaultMainWithSpec specs = do 
    trs <- mapM testSpecs specs 
    defaultMain (testGroup "tests" (concat trs))


lexerTests :: Spec
lexerTests = describe "LexerTest" $ do
    context "test whitespace" $ 
        it "return a EOF" $ 
            sTok " \t\v\r\n\f" `shouldBe` Right [CTokEof]
    context "test end" $
        it "return a EOF" $
            sTok "" `shouldBe` Right [CTokEof] 
    context "test nums" $ do 
        it "return a dec" $ 
            sTok "1239" `shouldBe` Right [(CTokInt 1239),CTokEof]
        it "return a 0x" $ 
            sTok "0x1000" `shouldBe` Right [(CTokInt 4096),CTokEof]
        it "return a 0X" $ 
            sTok "0X1000" `shouldBe` Right [(CTokInt 4096),CTokEof]
    context "test comment" $ do 
        it "test single comment" $
            sTok "//12313qweq[]()\":,.///**/" `shouldBe` Right [CTokEof]
        it "test single multi-line comment" $
            sTok "/* 111 */" `shouldBe` Right [CTokEof]
        it "test mul multi-line comment" $
            sTok "/* 123 \n  321 */" `shouldBe` Right [CTokEof]
        it "test embed multi-line comment" $
            sTok "/*123 /* qwe \n */*/" `shouldBe` Right [CTokEof]
        it "test embed multi-line comment fail" $
            sTok "/* 123 /*\n  321 */" `shouldSatisfy` isLeft
    context "test op" $ do 
        it "test +" $
            sTok  "+" `shouldBe` Right [CTokPlu,CTokEof]
        it "test -" $
            sTok  "-" `shouldBe` Right [CTokMin,CTokEof]
        it "test *" $
            sTok  "*" `shouldBe` Right [CTokMul,CTokEof]
        it "test /" $
            sTok  "/" `shouldBe` Right [CTokDiv,CTokEof]
        it "test %" $
            sTok  "%" `shouldBe` Right [CTokMod,CTokEof]
        it "test /=" $
            sTok  "=" `shouldBe` Right [CTokAss,CTokEof]
        it "test +=" $
            sTok  "+=" `shouldBe` Right [CTokPluAss,CTokEof]
        it "test -=" $
            sTok  "-=" `shouldBe` Right [CTokMinAss,CTokEof]
        it "test *=" $
            sTok  "*=" `shouldBe` Right [CTokMulAss,CTokEof]
        it "test /=" $
            sTok  "/=" `shouldBe` Right [CTokDivAss,CTokEof]
        it "test %=" $
            sTok  "%=" `shouldBe` Right [CTokModAss,CTokEof]
        it "test (" $
            sTok  "(" `shouldBe` Right [CTokLParen,CTokEof]
        it "test )" $
            sTok  ")" `shouldBe` Right [CTokRParen,CTokEof]
        it "test {" $
            sTok  "{" `shouldBe` Right [CTokLBrace,CTokEof]
        it "test }" $
            sTok  "}" `shouldBe` Right [CTokRBrace,CTokEof]
    context "test reserved ident" $ do 
        it "test struct" $
            sTok "struct" `shouldBe` Right [CTokRevsed "struct",CTokEof]
        it "test typedef" $
            sTok "typedef" `shouldBe` Right [CTokRevsed "typedef",CTokEof]
        it "test if" $
            sTok "if" `shouldBe` Right [CTokRevsed "if",CTokEof]
        it "test else" $
            sTok "else" `shouldBe` Right [CTokRevsed "else",CTokEof]
        it "test while" $
            sTok "while" `shouldBe` Right [CTokRevsed "while",CTokEof]
        it "test for" $
            sTok "for" `shouldBe` Right [CTokRevsed "for",CTokEof]
        it "test continue" $
            sTok "continue" `shouldBe` Right [CTokRevsed "continue",CTokEof]
        it "test break" $
            sTok "break" `shouldBe` Right [CTokRevsed "break",CTokEof]
        it "test assert" $
            sTok "assert" `shouldBe` Right [CTokRevsed "assert",CTokEof]
        it "test true" $
            sTok "true" `shouldBe` Right [CTokRevsed "true",CTokEof]
        it "test false" $
            sTok "false" `shouldBe` Right [CTokRevsed "false",CTokEof]
        it "test NULL" $
            sTok "NULL" `shouldBe` Right [CTokRevsed "NULL",CTokEof]
        it "test alloc" $
            sTok "alloc" `shouldBe` Right [CTokRevsed "alloc",CTokEof]
        it "test alloc_array" $
            sTok "alloc_array" `shouldBe` Right [CTokRevsed "alloc_array",CTokEof]
        it "test bool" $
            sTok "bool" `shouldBe` Right [CTokRevsed "bool",CTokEof]
        it "test void" $
            sTok "void" `shouldBe` Right [CTokRevsed "void",CTokEof]        
        it "test char" $
            sTok "char" `shouldBe` Right [CTokRevsed "char",CTokEof]
        it "test string" $
            sTok "string" `shouldBe` Right [CTokRevsed "string",CTokEof]
    context "test ident" $ do
        it "test main" $
            sTok "main" `shouldBe` Right [CTokMain,CTokEof]
        it "test int" $
            sTok "int" `shouldBe` Right [CTokType,CTokEof]
        it "test return" $
            sTok "return" `shouldBe` Right [CTokReturn ,CTokEof]
        it "test varname" $
            sTok "adada_01231_" `shouldBe` Right [CTokIdent "adada_01231_",CTokEof]
            