module Language.MicroPascal.Terminals where

data T_Type
  = TyInt | TyBool  
  deriving Eq

instance Show T_Type where
 show TyInt  = "integer"
 show TyBool = "boolean"

--

data T_UOp
  = Not | Neg
  deriving (Eq)

instance Show T_UOp where
  show Not = "not"
  show Neg = "-"

--

data T_BOp
  = Or | And | Equ | Less 
  | Plus | Minus | Mult | Div | Mod
  deriving (Eq)

instance Show T_BOp where
  show Or    = "or"
  show And   = "and"
  show Equ   = "="
  show Less  = "<"
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"
  show Div   = "/"
  show Mod   = "%"
