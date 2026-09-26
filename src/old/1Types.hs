module Types where

import Data.List (intercalate)

data LexType = LexOP                 -- (
            | LexCP                 -- )
            | LexNil
            | LexT
            | LexAtom String
            | LexStringAtom String
            | LexIntAtom Int
            | LexFloatAtom Float
            | LexQuote              -- '
            | LexPoint              -- .
            | LexAdd                -- +
            | LexSub                -- -
            | LexDiv                -- /
            | LexMul                -- *
            | LexEq                 -- =
            | LexGrt                -- >
            | LexGoE                -- >=
            | LexLss                -- <
            | LexLoE                -- <=
            | LexEof
            deriving (Show)

instance Eq LexType where
    LexOP == LexOP = True
    LexCP == LexCP = True
    LexNil == LexNil = True
    LexT == LexT = True
    LexAtom _ == LexAtom _ = True
    LexStringAtom _ == LexStringAtom _ = True
    LexIntAtom _ == LexIntAtom _ = True
    LexFloatAtom _ == LexFloatAtom _ = True
    LexQuote == LexQuote = True
    LexPoint == LexPoint = True
    LexAdd == LexAdd = True
    LexSub == LexSub = True
    LexDiv == LexDiv = True
    LexMul == LexMul = True
    LexEq == LexEq = True
    LexGrt == LexGrt = True
    LexGoE == LexGoE = True
    LexLss == LexLss = True
    LexLoE == LexLoE = True
    LexEof == LexEof = True
    _ == _ = False

data Lex = Lex {
    posSTR::Int,
    posSYM::Int, 
    lexType::LexType
} deriving (Show)

instance Eq Lex where
    (Lex n1 m1 t1) == (Lex n2 m2 t2) 
        | t1 == t2 = True
        | otherwise = False

data ExprTree a = Leaf a | Node [ExprTree a] deriving (Show)

data SExpr = SAtom Lex
            | SNil Lex
            | SList [SExpr]
    deriving (Show)

data Definition = Fun UserFun |
    Expr (ExprTree SExpr) deriving (Show)

data UserFun = UserFun{
    name::String,
    paramsList::[String],
    body::[(ExprTree SExpr)]
} deriving (Show)