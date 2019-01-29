-- PARSER

module Language.MicroPascal.Parser where

import Language.MicroPascal.AST
import Language.MicroPascal.Terminals
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token

parser :: String -> Either String Program
parser = either (Left . show) Right . parse programParser ""

programParser = do m_whiteSpace
                   m_reserved "program"
                   name <- m_identifier
                   m_semi  
                   defs <- defsparser 
                   body <- bodyparser
                   m_reservedOp "."
                   return (Program name (foldr ConsDefns NilDefns defs) body)



defsparser =  do m_reserved "var" 
                 vars <- many varparser
                 return vars 

varparser =   do v <- m_identifier
                 m_reservedOp ":" 
                 t <- typeparser
                 m_semi
                 return (VarDef v t) 

typeparser =     (m_reserved "integer" >> return TyInt)
             <|> (m_reserved "boolean" >> return TyBool)
                 


bodyparser :: Parser Stmts
bodyparser =  do m_reserved "begin" 
                 stmts <- m_semiSep stmtparser
                 m_reserved "end"
                 return (foldr ConsStmt NilStmt stmts) 

stmtparser :: Parser Stmt
stmtparser =      do  v  <- m_identifier
                      m_reservedOp ":="
                      e <- exprparser
                      return (Assig v e)
                     
              <|> do  m_reserved "if"
                      b <- exprparser
                      m_reserved "then"
                      ps <- bodyparser
                      m_reserved "else"
                      qs <- bodyparser
                      return (If b ps qs)


              <|> do  m_reserved "while"
                      b <- exprparser
                      m_reserved "do"
                      ps <- bodyparser
                      return (While b ps)

              <|> do  m_reserved "writeln"
                      e <- m_parens exprparser
                      return (Write e)
              <|> do  m_reserved "readln"
                      e <- m_parens m_identifier
                      return (Read e)


exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "not" >> return (Unary Not))]
        , [Prefix (m_reservedOp "-" >> return (Unary Neg))]
        , [Infix (m_reservedOp "and" >> return (Binary And)) AssocLeft]
        , [Infix (m_reservedOp "*" >> return (Binary Mult)) AssocLeft]
        , [Infix (m_reservedOp "div" >> return (Binary Div)) AssocLeft]
        , [Infix (m_reservedOp "mod" >> return (Binary Mod)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Binary Plus)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Binary Minus)) AssocLeft]
        , [Infix (m_reservedOp "or" >> return (Binary Or)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Binary Equ)) AssocLeft]
        , [Infix (m_reservedOp "<" >> return (Binary Less)) AssocLeft]
        ]

term = m_parens exprparser
       <|> (do vn  <- m_identifier 
               return (Var vn))
       <|> (m_reserved "true" >> return (BoolLit True))
       <|> (m_reserved "false" >> return (BoolLit False))
       <|> fmap IntLit m_natural

indexparser = do i <- m_brackets exprparser
                 return i

def :: LanguageDef st
def = emptyDef{ commentStart = "(*"
              , commentEnd = "*)"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "-<*+=:aondm."
              , opLetter = oneOf "-<*+=:andortivm."
              , reservedOpNames = [ "-", "<", "*", "+", "=", ":=",":"
                                  , "and", "or", "not","div","mod"]
              , reservedNames = ["true", "false"
                                , "program","var"
                                , "integer", "boolean"
                                , "begin","end"
                                , "if", "then", "else"
                                , "while", "do"
                                , "writeln", "readln"]
              , caseSensitive = False
              }

TokenParser{ parens     = m_parens
           , brackets   = m_brackets
           , semi       = m_semi
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved   = m_reserved
           , semiSep    = m_semiSep
           , semiSep1   = m_semiSep1
           , whiteSpace = m_whiteSpace
           , natural    = m_natural } = makeTokenParser def
