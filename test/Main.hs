module Main (main) where 
import LexerTest (lexerTests)
import ParserTest (parserTests)
import Util (defaultMainWithSpec)

main :: IO ()
main = defaultMainWithSpec [lexerTests,parserTests]





            