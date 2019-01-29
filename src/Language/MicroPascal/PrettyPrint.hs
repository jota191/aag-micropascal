{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}

module Language.MicroPascal.PrettyPrint where
import Language.MicroPascal.AST
import Language.MicroPascal.Sem
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.MicroPascal.Terminals
import Language.MicroPascal.Parser -- only for ghci TODO:


$(attLabels ["iind","scode"]) -- indentation leve, generated code string


-- | Top level aspect for pretty printing
asp_program_scode
  =  (p_Program   .=. program_scode )
 .*. emptyRecord 

program_scode (Fam chi par)
  = syndef scode ("PROGRAM " ++ (chi .# ch_pName #. lit @String ) ++ ";\n" ++
                  "VAR "     ++
                       (chi .# ch_pDefns #. scode) ++
                  "BEGIN\n"  ++
                       (chi .# ch_pBody #. scode) ++
                        
                  "END\n")



-- | scode for definitions
------------------------------------------------------------------------------
asp_PDefns_scode
  =  p_ConsDefns .=. consDfns_scode
 .*. p_NilDefns  .=. nilDefns_scode    
 .*. p_VarDef    .=. varDef_scode  
 .*. emptyRecord
 
consDfns_scode (Fam chi par)
  = syndef scode $ (chi .# ch_hdDfns #. scode) ++ "\n" ++
                   (chi .# ch_tlDfns #. scode)
                 
nilDefns_scode (Fam chi par)
  = syndef scode ""

varDef_scode (Fam chi par)
  = syndef scode $ (chi .# ch_defName #. lit @ String) ++ " : " ++
                   show (chi .# ch_defType #. lit @ T_Type)
-------------------------------------------------------------------------------                 


asp_stmts_scode
  =  p_NilStmt  .=. nilStmts_scode
 .*. p_ConsStmt .=. consStmt_scode
 .*. emptyRecord

nilStmts_scode (Fam chi par)
  = syndef scode ""

consStmt_scode (Fam chi par)
  = syndef scode $ (chi .# ch_hdStmt #. scode) ++ ";\n" ++
                   (chi .# ch_tlStmt #. scode) 


asp_Expr_scode
  = (p_Var    .=. \inp@(Fam chi par) -> syndef scode $
              (chi .# ch_varName #. lit @String)) --- WTF
 .*.
    (p_IntLit .=. \inp@(Fam chi par) -> syndef scode $
              show (chi .# ch_intLit #. lit @Integer)) -- TODO: error: (chi .# ch_intLit #. (show (lit @Integer)))
 .*.
    (p_BoolLit .=. \inp@(Fam chi par) -> syndef scode $
              show (chi .# ch_boolLit #. lit @Bool))
 .*.
    (p_Unary .=. \inp@(Fam chi par) -> syndef scode $
              (show (chi .# ch_uop #. (lit @T_UOp))) ++ " " ++
               (chi .# ch_uexp #. scode))
 .*.
    (p_Binary .=. \inp@(Fam chi par) -> syndef scode $
       "(" ++ (chi .# ch_lexp #. scode) ++ "" ++
              (show (chi .# ch_bop #. (lit @T_BOp))) ++ " " ++ --TODO: TUOP error
              (chi .# ch_rexp #. scode) ++ ")")
 .*. emptyRecord


asp_stmt_scode
  = (p_Assig .=. \inp@(Fam chi par) -> syndef scode $
      (chi .# ch_assigName #. lit @String)
        ++ " := "++
      (chi .# ch_assigExpr #. scode))
 .*.
    (p_If .=. \inp@(Fam chi par) -> syndef scode $
      "IF (" ++ (chi .# ch_ifCond #. scode) ++ ") THEN BEGIN\n" ++
          (chi .# ch_thenBody #. scode) ++
      "ELSE\n" ++
          (chi .# ch_elseBody #. scode) ++
      "END\n"
    )
 .*.
   (p_While .=. \inp@(Fam chi par) -> syndef scode $
     "WHILE (" ++ (chi .# ch_whileCond #. scode) ++ ") DO BEGIN\n" ++
        (chi .# ch_whileBody #. scode) ++
     "END\n"   
   )  
 .*.
  (p_Write .=. \inp@(Fam chi par) -> syndef scode $
     "WRITE " ++ (chi .# ch_writeExpr #. scode) ++ "\n"  
   )
 .*.
  (p_Read .=. \inp@(Fam chi par) -> syndef scode $
     "READ " ++ (chi .# ch_readExpr #. lit @String) ++ "\n"  
   )  
 .*.
    emptyRecord




asp_scode =  asp_Expr_scode .+. asp_stmt_scode .+. asp_stmts_scode .+.
             asp_PDefns_scode  .+. asp_program_scode 
  
pretty  p = sem_Program asp_scode p emptyAtt #. scode

-- p = Program {pName = "ejemplo2", pDefns = ConsDefns (VarDef {defName = "x", defType = TyInt}) (ConsDefns (VarDef {defName = "y", defType = TyBool}) ( ConsDefns ( VarDef {defName = "b", defType = TyBool}) NilDefns))}

-- p2 = Program {pName = "ejemplo2", pDefns = ConsDefns (VarDef {defName = "x", defType = TyInt}) (ConsDefns (VarDef {defName = "y", defType = TyBool}) ( ConsDefns ( VarDef {defName = "b", defType = TyBool}) NilDefns)),
--              pBody = ConsStmt (Assig "x" (Var "y")) (ConsStmt (Assig "tres" (Var "ke")) NilStmt)}

-- p = (Program {name = "ejemplo2", defns = [VarDef {defname = "x", defType = TyInt},VarDef {defname = "y", defType = TyInt},VarDef {defname = "b", defType = TyBool}], body = [Assig {assigName = "x", assigExpr = IntLit {intLit = 10}},Assig {assigName = "y", assigExpr = Binary {bop = Mult, lexp = Var {varName = "x"}, rexp = Binary {bop = Plus, lexp = IntLit {intLit = 3}, rexp = IntLit {intLit = 2}}}},Assig {assigName = "b", assigExpr = BoolLit {boolLit = True}},Assig {assigName = "b", assigExpr = Unary {uop = Not, uexp = Binary {bop = Less, lexp = Var {varName = "x"}, rexp = IntLit {intLit = 10}}}}]})
