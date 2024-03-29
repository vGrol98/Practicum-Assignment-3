module CSharpCode where

import Prelude hiding (LT, GT, EQ,(*>),(<*),(<$),($>))
import Data.Map as M hiding (map) 
import Data.Char (ord)
import Data.Maybe (fromJust)
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show

type Pointer = Int
type Environment = Map String Pointer

codeAlgebra :: CSharpAlgebra Code Code (Environment -> Code) (ValueOrAddress -> Environment -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatFor, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprUnaryOp, fExprMeth)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> (Environment -> Code) -> Code
fMembMeth t (LowerId x) ps s = [LABEL x, LINK 0] ++ s env ++ exitFunction env
    where
        env = fromList (zip (map getName ps) [-(n + 1) .. -2])
        getName (Decl _ (LowerId name)) = name
        n = length ps

fStatDecl :: Decl -> Environment -> Code
fStatDecl d env = []

fStatExpr :: (ValueOrAddress -> Environment -> Code) -> Environment -> Code
fStatExpr e env = e Value env ++ [pop]

fStatIf :: (ValueOrAddress -> Environment -> Code) 
    -> (Environment -> Code)
    -> (Environment -> Code)
    -> Environment
    -> Code
fStatIf e s1 s2 env = c ++ [BRF (n1 + 2)] ++ s1 env ++ [BRA n2] ++ s2 env
    where
        c        = e Value env
        (n1, n2) = (codeSize (s1 env), codeSize (s2 env))

fStatWhile :: (ValueOrAddress -> Environment -> Code) -> (Environment -> Code) -> Environment -> Code
fStatWhile e s1 env = [BRA n] ++ s1 env ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value env
        (n, k) = (codeSize (s1 env), codeSize c)

fStatFor :: (ValueOrAddress -> Environment -> Code) 
    -> (ValueOrAddress -> Environment -> Code)
    -> (ValueOrAddress -> Environment -> Code)
    -> (Environment -> Code)
    -> Environment
    -> Code
fStatFor init cond iter body env = fStatExpr init env ++ fStatWhile cond (\e -> body e ++ fStatExpr iter e) env

exitFunction :: Environment -> Code
exitFunction env = [UNLINK, STS (-n), AJS (-(n-1)), RET]
    where n = length env

fStatReturn :: (ValueOrAddress -> Environment -> Code) -> Environment -> Code
fStatReturn e env = e Value env ++ [STR R4] ++ exitFunction env

fStatBlock :: [Environment -> Code] -> Environment -> Code
fStatBlock cs env = concatMap ($env) cs

fExprCon :: Token -> ValueOrAddress -> Environment -> Code
fExprCon (ConstInt  n) va env = [LDC n]
fExprCon (ConstBool b) va env = [LDC (fromEnum b)]
fExprCon (ConstChar c) va env = [LDC (ord c)]

fExprVar :: Token -> ValueOrAddress -> Environment -> Code
fExprVar (LowerId id) va env = let loc = fromJust (M.lookup id env) in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

fLazyExprOp :: Instr -> (Int -> Instr) -> (ValueOrAddress -> Environment -> Code) -> (ValueOrAddress -> Environment -> Code) -> Environment -> Code
fLazyExprOp instr braInstr e1 e2 env = e1 Value env ++ [LDS 0, braInstr (codeSize second)] ++ second
    where second = e2 Value env ++ [instr]

fExprOp :: Token -> (ValueOrAddress -> Environment -> Code) -> (ValueOrAddress -> Environment -> Code) -> ValueOrAddress -> Environment -> Code
fExprOp (Operator "=") e1 e2 va env = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
fExprOp (Operator "&&") e1 e2 va env = fLazyExprOp AND BRF e1 e2 env
fExprOp (Operator "||") e1 e2 va env = fLazyExprOp OR BRT e1 e2 env
fExprOp (Operator op)  e1 e2 va env 
    | op `elem` assignmentOperators = fExprOp (Operator "=") e1 (fExprOp (Operator (init op)) e1 e2) va env -- a+=b -> a=a+b
    | otherwise = e1 Value env ++ e2 Value env ++ [opCodes ! op]

fExprUnaryOp :: BeforeOrAfter -> Token -> (ValueOrAddress -> Environment -> Code) -> ValueOrAddress -> Environment -> Code
fExprUnaryOp ba (Operator op) e va env = e Address env ++ [LDS 0, LDA 0] ++ doOp ba ++ [LDS (-2), STA 0, SWP, AJS (-1)]
    where 
        doOp After = [LDS 0, LDC 1, opCodes ![head op]]
        doOp Before = [LDC 1, opCodes ![head op], LDS 0]

fExprMeth :: Token -> [ValueOrAddress -> Environment -> Code] -> ValueOrAddress -> Environment -> Code
fExprMeth (LowerId id) args va env = concatMap (($env) . ($Value)) args ++ callMethod id
    where
        callMethod :: String -> Code
        -- Note: we can't just reverse the way the arguments are executed as that would change the order of evaluation
        callMethod "print" = concatMap ((:[TRAP 0]) . LDS) [-(length args) + 1 .. 0] ++ [LDC 0]
        callMethod id = [Bsr id, LDR R4]

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

