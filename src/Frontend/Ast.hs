module Frontend.Ast where

data Binop = 
      Add  
    | Min 
    | Mul 
    | Div 
    | Mod 
    deriving (Eq,Show)

data Asnop =
      Ass
    | AdA
    | MiA
    | MuA
    | DiA
    | MoA
    deriving (Eq,Show)

data PrefixExp = 
      Parens
    | Unary
    deriving (Eq,Show)

newtype Ident = CIdent String  deriving (Eq,Show) 

data ExpType = CInt deriving (Eq,Show)

data Exp = 
      GetPrefixExp PrefixExp Exp
    | Number Int
    | RIdent Ident 
    | GetBinop Binop Exp Exp
    deriving (Eq,Show)

newtype Lvalue = LIdent Ident 
    deriving (Eq,Show)

data Simp = Sp Lvalue Asnop Exp 
    deriving (Eq,Show)

data Decl = 
      Dec  ExpType Ident
    | Init ExpType Ident Exp 
    deriving (Eq,Show)

data Stml = 
      GetDecl Decl
    | GetSimp Simp
    | Return Exp 
    deriving (Eq,Show)

data Stmls = 
      GetBlock Block
    | MoreStmls Stml Stmls 
    deriving (Eq,Show)

newtype Block = GetStmls Stmls 
    deriving (Eq,Show)

newtype Program = Pg Block 
    deriving (Eq,Show)