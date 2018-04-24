module Util where 
import Test.Tasty
import Test.Tasty.Hspec
import Frontend.Lexer(scanner,CTok)
import Control.Monad(mapM)
import Frontend.Token (LexemeCLass(..))


scannerToTok:: Either String [CTok] -> Either String [LexemeCLass]
scannerToTok (Left a) = Left a 
scannerToTok (Right a) = Right (fmap fst a) 

sTok :: String -> Either String [LexemeCLass]
sTok = scannerToTok . scanner 

defaultMainWithSpec :: [Spec] -> IO ()
defaultMainWithSpec specs = do 
    trs <- mapM testSpecs specs 
    defaultMain (testGroup "tests" (concat trs))