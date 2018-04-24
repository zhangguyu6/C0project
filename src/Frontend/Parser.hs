{-# LANGUAGE FlexibleContexts #-}
module Frontend.Parser where 
import Prelude hiding(exp)
import Text.Parsec hiding (satisfy, string)
import qualified Text.Parsec.Expr as Ex
import Frontend.Lexer (scanner,CTok,AlexPosn(..))
import Frontend.Token (LexemeCLass(..))
import Frontend.Ast

-- 自定义token类型
type Parser = Parsec [CTok] ()

parseFromStr :: String -> Parser a -> Either String a 
parseFromStr s p = do 
        toks <- scanner s
        case runParser (p <* mkToParser CTokEof) () "fromStr" toks of 
            Left pe -> Left (show pe)
            Right a -> Right a

nextPos :: SourcePos -> CTok -> [CTok] -> SourcePos
nextPos pos _ ((_, AlexPn _ l c):_) = setSourceColumn (setSourceLine pos l) c
nextPos pos _ []                    = pos

satisfy' :: (CTok -> Maybe a) -> Parser a
satisfy' = tokenPrim show nextPos

satisfy :: (CTok -> Bool) -> Parser LexemeCLass
satisfy f= tokenPrim show nextPos istoken where
            istoken :: CTok -> Maybe LexemeCLass
            istoken t = if f t then Just (fst t) else Nothing

-- 根据LexemeCLass制造CTok
mkToParser :: LexemeCLass -> Parser LexemeCLass
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

-- 表达式
expr :: Parser Exp
expr  = Ex.buildExpressionParser table factor

parseExpFromStr :: String -> Either String Exp
parseExpFromStr s = parseFromStr s expr

-- 左值
lvalue :: Parser Lvalue
lvalue  = try (LIdent <$> ident) 
      <|> parens lvalue <?> "lvalue"

parseLvalueFromStr :: String -> Either String Lvalue
parseLvalueFromStr s= parseFromStr s lvalue

-- 赋值表达式
simp :: Parser Simp
simp = do 
    lv <- lvalue
    op <- asnop
    e <- expr 
    return $ assExten (Sp lv op e)

parseSimpFromStr :: String -> Either String Simp
parseSimpFromStr s= parseFromStr s simp

exptype :: Parser ExpType
exptype = (mkToParser CTokType) >> (return CInt)

decOnly :: Parser Decl 
decOnly = do 
    expt <- exptype 
    id <- ident
    return (Dec expt id)

decInit :: Parser Decl 
decInit = do 
    expt <- exptype 
    id <- ident
    mkToParser CTokAss
    e <- expr
    return (Init expt id e)

-- 声明表达式
decl :: Parser Decl
decl = (try decInit) 
      <|> decOnly <?> "decl"

parseDeclFromStr :: String -> Either String Decl
parseDeclFromStr s= parseFromStr s decl

-- 陈述
stml :: Parser Stml 
stml = (try (GetDecl <$> decl)
      <|> try (GetSimp <$> simp)
      <|> (mkToParser CTokReturn >> Return <$> expr)) <* semicolon <?> "stml"

parseStmlFromStr :: String -> Either String Stml
parseStmlFromStr s= parseFromStr s stml

-- 块
block :: Parser Block 
block = GetStmls <$> braces stmls <?> "block"

parseBlockFromStr :: String -> Either String Block
parseBlockFromStr s= parseFromStr s block

morestmls :: Parser Stmls 
morestmls = do 
    s <- stml
    ss <- stmls 
    return $ MoreStmls s ss  

-- 多重陈述
stmls :: Parser Stmls 
stmls = try (GetBlock <$> block)
      <|> try morestmls
      <|> GetOneStml <$> stml <?> "stmls"

parseStmlsFromStr :: String -> Either String Stmls
parseStmlsFromStr s= parseFromStr s stmls

-- 程序
progm :: Parser Program
progm = do 
    mkToParser CTokType 
    mkToParser CTokMain
    mkToParser CTokLParen
    mkToParser CTokRParen
    Pg <$> block <?> "progm"

parsePgFromStr :: String -> Either String Program
parsePgFromStr s= parseFromStr s progm

factor :: Parser Exp
factor = try intconst
      <|> identexp
      <|> parens expr <?> "factor"

binary s f = Ex.Infix (mkToParser s >> return (GetBinop f))
prefix s f = Ex.Prefix (mkToParser s >> return (GetPrefixExp f))

table = [ [prefix CTokMin Unary]
        , [binary CTokMul Mul Ex.AssocLeft,binary CTokDiv Div Ex.AssocLeft,binary CTokMod Mod Ex.AssocLeft]
        , [binary CTokPlu Add Ex.AssocLeft,binary CTokMin Min Ex.AssocLeft]]

reserved :: Parser LexemeCLass
reserved = satisfy' p <?> "reserved"
    where p (t,_) = case t of 
                  tt@(CTokRevsed a) -> Just tt
                  _ -> Nothing

asnop :: Parser Asnop
asnop = satisfy' p <?> "asnop"
    where   p (CTokAss,_) = Just Ass
            p (CTokPluAss,_) = Just AdA
            p (CTokMinAss,_) = Just MiA
            p (CTokMulAss,_) = Just MuA
            p (CTokModAss,_) = Just MoA
            p (CTokDivAss,_) = Just DiA
            p _  = Nothing

binop :: Parser Binop
binop = satisfy' p <?> "binop"
    where p (CTokPlu,_) = Just Add
          p (CTokMin,_) = Just Min
          p (CTokMul,_) = Just Mul
          p (CTokDiv,_) = Just Div
          p (CTokMod,_) = Just Mod
          p _  = Nothing

parens :: Parser a -> Parser a
parens = between (mkToParser CTokLParen) (mkToParser CTokRParen)

braces :: Parser a -> Parser a
braces = between (mkToParser CTokLBrace) (mkToParser CTokRBrace)

semicolon :: Parser LexemeCLass
semicolon = mkToParser CTokSem

