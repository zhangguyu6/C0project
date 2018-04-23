{
module Frontend.Lexer 
(scanner,CTok,AlexPosn(..))
  where
import Control.Monad (when)
import Frontend.Token (LexemeCLass(..))
}
%wrapper "monadUserState"

$whitespace = [\ \t\v\r\n\f]
@decnum = 0 | [1-9][0-9]*
@hexnum = 0[xX][0-9a-fA-F]+
@num = @decnum | @hexnum
@ident = [A-Za-z_][A-Za-z0-9_]*



c0token :-
  <0> $white+ ;
  <0> @ident {ident}
  <0> @num {tokWV stringToNum}
  <0> "//" {enterSinglelineComment `andBegin` single_comment}
  <single_comment> . # \n ;
  <single_comment> \n {skip `andBegin` state_initial}
  <0> "/*" {enterComment `andBegin` mul_comment}
  <mul_comment> "/*" {embedComment}
  <mul_comment> "*/" {unembedComment}
  <mul_comment> . ;
  <mul_comment> \n {skip}
  <0> \+ {tok CTokPlu}
  <0> \- {tok CTokMin}
  <0> \* {tok CTokMul}
  <0> \/ {tok CTokDiv}
  <0> \% {tok CTokMod}
  <0> \= {tok CTokAss}
  <0> "+=" {tok CTokPluAss}
  <0> "-=" {tok CTokMinAss}
  <0> "*=" {tok CTokMulAss}
  <0> "/=" {tok CTokDivAss}
  <0> "%=" {tok CTokModAss}
  <0> \(   {tok CTokLParen}
  <0> \)   {tok CTokRParen}
  <0> \{   {tok CTokLBrace}
  <0> \}   {tok CTokRBrace}

{

-- (token,位置)
type CTok = (LexemeCLass,AlexPosn)

tok :: LexemeCLass -> AlexInput -> Int -> Alex CTok
tok  t (p,_,_,_) _ = return (t,p)

tokWV :: (String -> LexemeCLass) -> AlexInput -> Int -> Alex CTok
tokWV f (p,_,_,s) len = return (f (take len s),p)

ident :: AlexInput -> Int -> Alex CTok
ident (p,_,_,s) len = return (t,p)
      where 
        ident' = take len s
        t = case ident' of 
                "struct" -> CTokRevsed ident'
                "typedef" -> CTokRevsed ident'
                "if" -> CTokRevsed ident' 
                "else" -> CTokRevsed ident' 
                "while" -> CTokRevsed ident'  
                "for" -> CTokRevsed ident' 
                "continue" -> CTokRevsed ident' 
                "break" -> CTokRevsed ident' 
                "return" -> CTokReturn
                "assert" -> CTokRevsed ident' 
                "true" -> CTokRevsed ident' 
                "false"  -> CTokRevsed ident' 
                "NULL" -> CTokRevsed ident' 
                "alloc" -> CTokRevsed ident'  
                "alloc_array" -> CTokRevsed ident' 
                "int" -> CTokType 
                "bool" -> CTokRevsed ident'  
                "void" -> CTokRevsed ident'  
                "char" -> CTokRevsed ident'  
                "string" -> CTokRevsed ident'
                "main" -> CTokMain
                _ -> CTokIdent ident'


stringToNum :: String -> LexemeCLass                
stringToNum s = CTokInt (read s :: Int)

-- user-specified routine for monadUserState
data AlexUserState = AlexUserState
                   {
                       lexerCommentDepth  :: Int
                    --  , lexerStringValue   :: String
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                       lexerCommentDepth  = 0
                    --  , lexerStringValue   = ""
                   }

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

embedComment :: AlexInput -> Int -> Alex CTok
embedComment input len =
  do cd <- getLexerCommentDepth
     setLexerCommentDepth (cd + 1)
     skip input len

unembedComment :: AlexInput -> Int -> Alex CTok
unembedComment input len =
  do  cd <- getLexerCommentDepth
      setLexerCommentDepth (cd - 1)
      when (cd == 1) (alexSetStartCode state_initial)
      skip input len

alexEOF :: Alex CTok
alexEOF = return (CTokEof,AlexPn (-1) (-1) (-1))

state_initial :: Int
state_initial = 0

enterSinglelineComment :: AlexInput -> Int -> Alex CTok
enterSinglelineComment input len = skip input len

enterComment :: AlexInput -> Int -> Alex CTok
enterComment input len = do setLexerCommentDepth 1
                            skip input len

scanner :: String -> Either String [CTok]
scanner str = runAlex str loop
  where loop = do
          t@(tok, _) <- alexMonadScan
          if tok == CTokEof
            then do d <- getLexerCommentDepth
                    if d /= 0
                      then alexError "Comment not closed at end of file"
                      else return [t]
            else do toks <- loop
                    return (t:toks)

}