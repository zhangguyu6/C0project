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
       Unary
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
      GetOneStml Stml
    | GetBlock Block
    | MoreStmls Stml Stmls 
    deriving (Eq,Show)

newtype Block = GetStmls Stmls 
    deriving (Eq,Show)

newtype Program = Pg Block 
    deriving (Eq,Show)

assExten :: Simp -> Simp 
assExten simp@(Sp lv Ass e) = simp
assExten (Sp lv AdA e) = Sp lv Ass (GetBinop Add (lvalueTorvalue lv) e)
assExten (Sp lv MiA e) = Sp lv Ass (GetBinop Min (lvalueTorvalue lv) e)
assExten (Sp lv MuA e) = Sp lv Ass (GetBinop Mul (lvalueTorvalue lv) e)
assExten (Sp lv DiA e) = Sp lv Ass (GetBinop Div (lvalueTorvalue lv) e)
assExten (Sp lv MoA e) = Sp lv Ass (GetBinop Mod (lvalueTorvalue lv) e)

lvalueTorvalue :: Lvalue -> Exp
lvalueTorvalue (LIdent l)= (RIdent l) 