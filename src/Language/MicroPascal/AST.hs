-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DE SINTAXIS DEL LENGUAJE

-- Incluye los tipos que definen el AST del
-- lenguaje y una funcion parser, que permite
-- leer una cadena de caracteres que contenga
-- a un programa y retorna su AST, si este es
-- sintacticamente valido, o un mensaje de error

module Language.MicroPascal.AST where
import Language.Grammars.AspectAG
import Language.MicroPascal.Terminals

-- AST del lenguaje

type Name  = String

data Defns = NilDefns {}
           | ConsDefns { hdDfns :: VarDef , tlDfns :: Defns}
            deriving (Show)


data Program = Program { pName   :: String, -- ,
                         pDefns  :: Defns, --  ,
                         pBody   :: Stmts} 
             deriving Show

data List a = Nil {} | Cons {hd :: a, tl :: List a } deriving (Show, Eq,Ord)

--type Defns = [VarDef]


data VarDef  = VarDef { defName :: String,
                        defType :: T_Type}
             deriving Show



data Stmts
  = NilStmt {} | ConsStmt {hdStmt :: Stmt, tlStmt :: Stmts}
  deriving Show

data Stmt = Assig {assigName :: String, assigExpr :: Expr}
          | If    {ifCond :: Expr, thenBody :: Stmts, elseBody :: Stmts}
          | While {whileCond :: Expr, whileBody :: Stmts}
          | Write {writeExpr :: Expr}
          | Read  {readExpr  :: String}
  deriving Show

data Expr = Var     {varName :: String}
          | IntLit  {intLit  :: Integer}
          | BoolLit {boolLit :: Bool}
          | Unary   {uop :: T_UOp, uexp :: Expr}
          | Binary  {bop :: T_BOp, lexp :: Expr, rexp :: Expr}
 deriving (Eq,Show)
