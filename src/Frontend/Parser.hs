{-# LANGUAGE FlexibleContexts #-}
module Frontend.Parser where 
import Prelude hiding(exp)
import Text.Parsec hiding (satisfy, string)
import Frontend.Lexer (CTok,AlexPosn(..))
import Frontend.Token (LexemeCLass(..))
import Frontend.Ast

-- 自定义token类型
type Parser = Parsec [CTok] ()

nextPos :: SourcePos -> CTok -> [CTok] -> SourcePos
nextPos pos _ ((_, AlexPn _ l c):_) = setSourceColumn (setSourceLine pos l) c
nextPos pos _ []                    = pos

satisfy' :: (Stream [CTok] m CTok) => (CTok -> Maybe a) -> ParsecT [CTok] u m a
satisfy' = tokenPrim show nextPos

satisfy :: (Stream [CTok] m CTok) => (CTok -> Bool) -> ParsecT [CTok] u m LexemeCLass
satisfy f= tokenPrim show nextPos istoken where
            istoken :: CTok -> Maybe LexemeCLass
            istoken t = if f t then Just (fst t) else Nothing

-- 根据LexemeCLass制造CTok
mkToParser :: (Stream [CTok] m CTok) => LexemeCLass -> ParsecT [CTok] u m LexemeCLass
mkToParser tok = satisfy (\(t,_)->t == tok) <?> show tok

-- 终结符
-- 数字表达式
intconst :: Parser Exp
intconst = satisfy' p <?> "number"
    where p (t,_) = case t of 
                      CTokInt a -> Just (Number a)
                      _ -> Nothing
-- 标识符
ident :: Parser Ident
ident = satisfy' p <?> "ident"
    where p (t,_) = case t of 
                      CTokIdent a -> Just (CIdent a)
                      _ -> Nothing
-- 标识符表达式
identexp :: Parser Exp
identexp = RIdent <$> ident

-- 二元操作表达式
binexp :: Parser Exp
binexp = do 
    e1 <- exp
    op <- binop
    e2 <- exp
    return (GetBinop op e1 e2)

-- (表达式)
parensexp :: Parser Exp
parensexp = GetPrefixExp Parens <$> parens exp

-- -表达式
minsexp :: Parser Exp
minsexp = do 
    mkToParser CTokMin
    e <- exp
    return (GetPrefixExp Unary e)

-- 表达式
exp :: Parser Exp
exp = intconst <|> identexp <|> binexp <|> parensexp <|> minsexp

reserved :: Parser LexemeCLass
reserved = satisfy' p <?> "reserved"
    where p (t,_) = case t of 
                  tt@(CTokRevsed a) -> Just tt
                  _ -> Nothing

asnop :: Parser LexemeCLass
asnop = satisfy' p <?> "asnop"
    where p (t,_) 
            | t `elem` [CTokAss,CTokPluAss,CTokMinAss,CTokMulAss,CTokDivAss,CTokModAss] = Just t
            | otherwise = Nothing

binop :: Parser Binop
binop = satisfy' p <?> "binop"
    where p (CTokPlu,_) = Just Add
          p (CTokMin,_) = Just Min
          p (CTokMul,_) = Just Mul
          p (CTokDiv,_) = Just Div
          p (CTokMod,_) = Just Mod
          p _  = Nothing

parens :: Monad m => ParsecT [CTok] u m a -> ParsecT [CTok] u m a
parens = between (mkToParser CTokLParen) (mkToParser CTokRParen)

braces :: Monad m => ParsecT [CTok] u m a -> ParsecT [CTok] u m a
braces = between (mkToParser CTokLBrace) (mkToParser CTokRBrace)

