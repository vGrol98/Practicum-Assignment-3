module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import Prelude hiding ((*>),(<*),(<$),($>))
import CSharpLex


data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data BeforeOrAfter = Before | After deriving (Eq,Show)

data Expr = ExprConst       Token
          | ExprVar         Token
          | ExprOper        Token Expr Expr
          | ExprUnaryOper   BeforeOrAfter Token Expr
          | ExprMeth        Token [Expr]
          deriving Show

data Decl = Decl Type Token

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving Eq

instance Show Decl where
    show (Decl t (LowerId n)) = show t ++ " " ++ n

instance Show Type where
    show TypeVoid = "void"
    show (TypePrim (StdType t)) = t
    show (TypeObj (UpperId t)) = t
    show (TypeArray t) = show t ++ "[]"


parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)

pExprUnaryOp :: [([Token], BeforeOrAfter)] -> Parser Token Expr
pExprUnaryOp [] = pExprSimple
pExprUnaryOp ((p,Before):ps) = ($) <$> option (ExprUnaryOper Before <$> (choice (map symbol p))) id <*> pExprUnaryOp ps
pExprUnaryOp ((p,After):ps) = flip ($) <$> pExprUnaryOp ps <*> option (ExprUnaryOper After <$> (choice (map symbol p))) id

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr
           <|> ExprMeth  <$> sLowerId <*> parenthesised (option (listOf pExpr (symbol Comma)) [])

pExprOp :: [[Token]] -> Parser Token Expr
pExprOp [] = pExprUnaryOp [(map Operator crementOperators,Before),(map Operator crementOperators,After)]
pExprOp (p:ps) = chainl (pExprOp ps) (ExprOper <$> choice (map symbol p))

pExpr :: Parser Token Expr
pExpr = chainr (pExprOp operatorPriorities) (ExprOper <$> (choice (map (symbol . Operator) assignmentOperators)))

operatorPriorities :: [[Token]]
operatorPriorities = reverse $ map (map Operator) [["*","/","%"],["+","-"],["<",">","<=",">="],["==","!="],["^"],["&&"],["||"]]

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])


pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)


pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))


pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

