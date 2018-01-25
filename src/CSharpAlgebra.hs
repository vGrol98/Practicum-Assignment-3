module CSharpAlgebra where

import Prelude hiding ((*>),(<*),(<$),($>))
import CSharpLex
import CSharpGram
import Data.List


type CSharpAlgebra clas memb stat expr
    = (  Token -> [memb] -> clas

      ,  ( Decl                             -> memb
         , Type -> Token -> [Decl] -> stat  -> memb
         )

      ,  ( Decl                          -> stat
         , expr                          -> stat
         , expr -> stat -> stat          -> stat
         , expr -> stat                  -> stat
         , expr -> expr -> expr -> stat  -> stat
         , expr                          -> stat
         , [stat]                        -> stat
         )

      ,  ( Token                  -> expr
         , Token                  -> expr
         , Token -> expr -> expr  -> expr
         , BeforeOrAfter -> Token -> expr -> expr
         , Token -> [expr]        -> expr
         )
      )


foldCSharp :: CSharpAlgebra clas memb stat expr -> Class -> clas
foldCSharp (c1, (m1,m2), (s1,s2,s3,s4,s5,s6,s7), (e1,e2,e3,e4,e5)) = fClas
    where
        fClas (Class      c ms)     = c1 c (map fMemb ms)
        fMemb (MemberD    d)        = m1 d
        fMemb (MemberM    t m ps s) = m2 t m ps (fStat s)
        fStat (StatDecl   d)        = s1 d
        fStat (StatExpr   e)        = s2 (fExpr e)
        fStat (StatIf     e s1 s2)  = s3 (fExpr e) (fStat s1) (fStat s2)
        fStat (StatWhile  e s1)     = s4 (fExpr e) (fStat s1)
        fStat (StatFor    i e it s) = s5 (fExpr i) (fExpr e) (fExpr it) (fStat s)
        fStat (StatReturn e)        = s6 (fExpr e)
        fStat (StatBlock  ss)       = s7 (map fStat ss)
        fExpr (ExprConst  con)      = e1 con
        fExpr (ExprVar    var)      = e2 var
        fExpr (ExprOper   op e1 e2) = e3 op (fExpr e1) (fExpr e2)
        fExpr (ExprUnaryOper ba op e) = e4 ba op (fExpr e)
        fExpr (ExprMeth   meth es)  = e5 meth (map fExpr es)

formatAlgebra :: CSharpAlgebra [String] [String] [String] (String,Bool)
formatAlgebra =
    ( formatClas
    , (formatMembDecl, formatMembMeth)
    , (formatStatDecl, formatStatExpr, formatStatIf, formatStatWhile, formatStatFor, formatStatReturn, formatStatBlock)
    , (formatExprCon, formatExprVar, formatExprOp, formatExprUnaryOp, formatExprMeth)
    )

indent :: [String] -> [String]
indent = map ('\t':)

formatClas :: Token -> [[String]] -> [String]
formatClas (UpperId c) ms = ["class " ++ c, "{"] ++ indent (concat ms) ++ ["}"]

formatMembDecl :: Decl -> [String]
formatMembDecl d = [show d ++ ";"]

formatMembMeth :: Type -> Token -> [Decl] -> [String] -> [String]
formatMembMeth t (LowerId name) ps content = [show t ++ " " ++ name ++ " (" ++ intercalate ", " (map show ps) ++ ")"] ++ content

formatStatDecl :: Decl -> [String]
formatStatDecl d = [show d ++ ";"]

formatStatExpr :: (String, Bool) -> [String]
formatStatExpr (e, _) = [e ++ ";"]

formatStatIf :: (String, Bool) -> [String] -> [String] -> [String]
formatStatIf (condition, _) thn els = 
    ["if (" ++ condition ++ ")"] ++ 
        indent thn ++ 
        if length els == 2 -- empty block
            then [] 
            else ["else"] ++ indent els

formatStatWhile :: (String, Bool) -> [String] -> [String]
formatStatWhile (condition, _) thn = ["while (" ++ condition ++ ")"] ++ indent thn

formatStatFor :: (String, Bool) -> (String, Bool) -> (String, Bool) -> [String] -> [String]
formatStatFor (init,_) (condition,_) (iter,_) body = ["for (" ++ init ++ "; " ++ condition ++ "; " ++ iter ++ ") "] ++ indent body

formatStatReturn :: (String, Bool) -> [String]
formatStatReturn (e, _) = ["return " ++ e ++ ";"]

formatStatBlock :: [[String]] -> [String]
formatStatBlock content = ["{"] ++ indent (concat content) ++ ["}"] 

formatExprCon :: Token -> (String, Bool)
formatExprCon (ConstInt n) = (show n, False)
formatExprCon (ConstBool b) = (if b then "true" else "false", False)
formatExprCon (ConstChar c) = ("'" ++ [c] ++ "'", False)

formatExprVar :: Token -> (String, Bool)
formatExprVar (LowerId id) = (id, False)

addParentheses :: (String, Bool) -> String
addParentheses (s, False) = s
addParentheses (s, True)  = "(" ++ s ++ ")"

formatExprOp :: Token -> (String, Bool) -> (String, Bool) -> (String, Bool)
formatExprOp (Operator op) left@(_, lb) right@(_, rb) = (addParentheses left ++ " " ++ op ++ " " ++ addParentheses right, not (lb || rb))

formatExprUnaryOp :: BeforeOrAfter -> Token -> (String, Bool) -> (String,Bool)
formatExprUnaryOp ba (Operator op) e = (combineWithOp ba (addParentheses e), True)
    where
        combineWithOp Before s = op ++ s
        combineWithOp After s = s ++ op

formatExprMeth :: Token -> [(String,Bool)] -> (String, Bool)
formatExprMeth (LowerId name) args = (name ++ "(" ++ intercalate ", " (map fst args) ++ ")", False)
