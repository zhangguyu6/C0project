module Frontend.Token 
(LexemeCLass(..))
where


data LexemeCLass =
      CTokEof  -- eof of flow
    | CTokPlu  -- +
    | CTokMin  -- -
    | CTokMul  -- *
    | CTokDiv  -- /
    | CTokMod  -- %
    | CTokAss  -- =
    | CTokPluAss -- +=
    | CTokMinAss -- -=
    | CTokMulAss -- *=
    | CTokDivAss -- /=
    | CTokModAss -- %=
    | CTokLParen -- (
    | CTokRParen -- )
    | CTokLBrace -- {
    | CTokRBrace -- }
    | CTokType -- int 类型声明
    | CTokMain -- main
    | CTokReturn -- return
    | CTokRevsed String-- reversed keywords
    | CTokInt Int --32bit word for decnum and hexnum
    | CTokIdent String -- ident
  deriving Eq

instance Show LexemeCLass where 
  show CTokEof  = "eof"
  show CTokPlu  = "+"
  show CTokMin  = "-"
  show CTokMul  = "*"
  show CTokDiv  = "/"
  show CTokMod  = "%"
  show CTokAss  = "="
  show CTokPluAss = "+="
  show CTokMinAss = "-="
  show CTokMulAss = "*="
  show CTokDivAss = "/="
  show CTokModAss = "%="
  show CTokLParen = "("
  show CTokRParen = ")"
  show CTokLBrace = "{"
  show CTokRBrace = "}"
  show CTokType = "int" -- int 类型声明
  show CTokMain = "main"
  show CTokReturn = "return"
  show (CTokRevsed a) = "revsed" ++ a-- reversed keywords
  show (CTokInt a) = show a--32bit word for decnum and hexnum
  show (CTokIdent a) = "idnet:"++a -- ident