module CSharpCode where

import Prelude hiding (LT, GT, EQ,(*>),(<*),(<$),($>))
import Data.Map as M
import Data.Char (ord)
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show

type Pointer = Int
type Environment = Map Token Pointer

codeAlgebra :: CSharpAlgebra Code Code (Environment -> Code) (ValueOrAddress -> Environment -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprMeth)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> Code -> Code
fMembMeth t (LowerId x) ps s = [LABEL x] ++ s ++ [RET]

fStatDecl :: Decl -> Environment -> Code
fStatDecl d env = []

fStatExpr :: (ValueOrAddress -> Environment -> Code) -> Environment -> Code
fStatExpr e env = e Value env ++ [pop]

fStatIf :: (ValueOrAddress -> Environment -> Code) -> (Environment -> Code) -> (Environment -> Code) -> Environment -> Code
fStatIf e s1 s2 env = c ++ [BRF (n1 + 2)] ++ s1 env ++ [BRA n2] ++ s2 env
    where
        c        = e Value env
        (n1, n2) = (codeSize (s1 env), codeSize (s2 env))

fStatWhile :: (ValueOrAddress -> Environment -> Code) -> (Environment -> Code) -> Environment -> Code
fStatWhile e s1 env = [BRA n] ++ s1 env ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value env
        (n, k) = (codeSize (s1 env), codeSize c)

fStatReturn :: (ValueOrAddress -> Environment -> Code) -> Environment -> Code
fStatReturn e env = e Value env ++ [pop] ++ [RET]

fStatBlock :: [Environment -> Code] -> Environment -> Code
fStatBlock = concat

fExprCon :: Token -> ValueOrAddress -> Environment -> Code
fExprCon (ConstInt  n) va env = [LDC n]
fExprCon (ConstBool b) va env = [LDC (fromEnum b)]
fExprCon (ConstChar c) va env = [LDC (ord c)]

fExprVar :: Token -> ValueOrAddress -> Environment -> Code
fExprVar id va env = let loc = fromJust (M.lookup id env) in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

fExprOp :: Token -> (ValueOrAddress -> Environment -> Code) -> (ValueOrAddress -> Environment -> Code) -> ValueOrAddress -> Environment -> Code
fExprOp (Operator "=") e1 e2 va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp (Operator op)  e1 e2 va = e1 Value ++ e2 Value ++ [opCodes ! op]

-- Assignment 4 - TODO
fExprMeth :: Token -> [ValueOrAddress -> Environment -> Code] -> ValueOrAddress -> Environment -> Code
fExprMeth = 


opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

