module CSharpFormat where

import Data.List
import CSharpLex
import CSharpGram
import CSharpAlgebra

formatAlgebra :: CSharpAlgebra [String] [String] [String] (String,Bool)
formatAlgebra =
    ( formatClas
    , (formatMembDecl, formatMembMeth)
    , (formatStatDecl, formatStatExpr, formatStatIf, formatStatWhile, formatStatReturn, formatStatBlock)
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
formatStatIf (condition, _) thn els = ["if (" ++ condition ++ ")"] ++ indent thn ++ ["else"] ++ indent els

formatStatWhile :: (String,Bool) -> [String] -> [String]
formatStatWhile (condition, _) thn = ["while (" ++ condition ++ ")"] ++ indent thn

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
