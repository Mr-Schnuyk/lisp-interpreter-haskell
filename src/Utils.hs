module Utils
    ( getEl
    , takeL
    , eqLexTypes
    , getStr
    , getSym
    , allLeaf
    , isEqExprTree
    , isEqExpr
    , isEqLex
    , isEqLexHelp
    , isFloat
    , isInt
    , getInt
    , getFloat
    , deepSeqTree
    , eval
    , evalList
    , showLex
    , showSExpr
    , replaceLeafs
    , replaceLeafLst
    , flattenTree
    , flattenTreeHelp
    ) where

import Types
import Data.List (intercalate)

-- Вспомогательная функция для получения позиции из ExprTree
getPosFromTree :: ExprTree SExpr -> (Int, Int)
getPosFromTree (Leaf (SAtom (Lex line col _))) = (line, col)
getPosFromTree (Leaf (SNil (Lex line col _))) = (line, col)
getPosFromTree (Leaf (SList _)) = (-1, -1)
getPosFromTree (Node _) = (-1, -1)

getEl :: [a] -> Int -> Either String a
getEl [] _ = Left "Error: Trying to get an element from an empty list"
getEl (x:xl) n
    | n == 0 = Right x
    | n > 0 = getEl xl (n - 1)
    | otherwise = getEl (x:xl) (length (x:xl) + n)

takeL :: [a] -> Int -> Int -> [a]
takeL [] _ _ = []
takeL lst fpos lpos
    | fpos < 0 = takeL lst (fpos + length lst) lpos
    | lpos < 0 = takeL lst fpos (lpos + length lst)
    | fpos > lpos = takeL lst lpos fpos
    | otherwise = takeL' lst fpos lpos
  where
    takeL' (x:_) 0 0 = [x]
    takeL' (x:xl) 0 n = x : takeL' xl 0 (n - 1)
    takeL' (_:xl) n m = takeL' xl (n - 1) (m - 1)
    takeL' [] _ _ = []

eqLexTypes :: Lex -> LexType -> Bool
eqLexTypes (Lex _ _ t1) t2 = t1 == t2

getStr :: Lex -> Int
getStr (Lex n _ _) = n

getSym :: Lex -> Int
getSym (Lex _ m _) = m

allLeaf :: [ExprTree a] -> Bool
allLeaf [] = True
allLeaf (Leaf _ : xl) = allLeaf xl
allLeaf (Node _ : _) = False

isEqExprTree :: ExprTree SExpr -> ExprTree SExpr -> Bool
isEqExprTree (Node lst1) (Node lst2)
    | length lst1 == length lst2 =
        foldl (&&) True (zipWith isEqExprTree lst1 lst2)
    | otherwise = False
isEqExprTree (Leaf expr1) (Leaf expr2) = isEqExpr expr1 expr2
isEqExprTree _ _ = False

isEqExpr :: SExpr -> SExpr -> Bool
isEqExpr (SAtom lex1) (SAtom lex2) = isEqLex lex1 lex2
isEqExpr (SNil _) (SNil _) = True
isEqExpr (SList lst1) (SList lst2)
    | length lst1 == length lst2 =
        foldl (&&) True (zipWith isEqExpr lst1 lst2)
    | otherwise = False
isEqExpr _ _ = False

isEqLex :: Lex -> Lex -> Bool
isEqLex (Lex _ _ t1) (Lex _ _ t2)
    | t1 == t2 = isEqLexHelp t1 t2
    | otherwise = False

isEqLexHelp :: LexType -> LexType -> Bool
isEqLexHelp (LexAtom str1) (LexAtom str2) = str1 == str2
isEqLexHelp (LexStringAtom str1) (LexStringAtom str2) = str1 == str2
isEqLexHelp (LexIntAtom n1) (LexIntAtom n2) = n1 == n2
isEqLexHelp (LexFloatAtom f1) (LexFloatAtom f2) = f1 == f2
isEqLexHelp _ _ = True

isFloat :: ExprTree SExpr -> Either String Bool
isFloat (Leaf (SAtom (Lex _ _ (LexFloatAtom _)))) = Right True
isFloat (Leaf (SAtom (Lex _ _ (LexIntAtom _)))) = Right False
isFloat tree =
    let (line, col) = getPosFromTree tree
    in Left $ "Это не является числом at " ++ show line ++ "::" ++ show col

isInt :: ExprTree SExpr -> Either String Bool
isInt (Leaf (SAtom (Lex _ _ (LexFloatAtom _)))) = Right False
isInt (Leaf (SAtom (Lex _ _ (LexIntAtom _)))) = Right True
isInt tree =
    let (line, col) = getPosFromTree tree
    in Left $ "Это не является числом at " ++ show line ++ "::" ++ show col

getInt :: ExprTree SExpr -> Either String Int
getInt (Leaf (SAtom (Lex _ _ (LexIntAtom num)))) = Right num
getInt tree =
    let (line, col) = getPosFromTree tree
    in Left $ "Это не является числом at " ++ show line ++ "::" ++ show col

getFloat :: ExprTree SExpr -> Either String Float
getFloat (Leaf (SAtom (Lex _ _ (LexFloatAtom num)))) = Right num
getFloat tree =
    let (line, col) = getPosFromTree tree
    in Left $ "Это не является числом at " ++ show line ++ "::" ++ show col

deepSeqTree :: [ExprTree a] -> Either String (ExprTree a)
deepSeqTree [] = Left "deepSeqTree: empty list"
deepSeqTree (x:xs) = Right (foldl (\_ tree -> eval tree `seq` tree) x xs)

eval :: ExprTree a -> ()
eval (Leaf x)   = x `seq` ()
eval (Node children) = evalList children `seq` ()

evalList :: [ExprTree a] -> ()
evalList [] = ()
evalList (t:ts) = eval t `seq` evalList ts

showLex :: Lex -> String
showLex (Lex _ _ LexOP) = "("
showLex (Lex _ _ LexCP) = ")"
showLex (Lex _ _ LexNil) = "nil"
showLex (Lex _ _ LexT) = "T"
showLex (Lex _ _ (LexAtom str)) = str
showLex (Lex _ _ (LexStringAtom str)) = "\"" ++ str ++ "\""
showLex (Lex _ _ (LexIntAtom n)) = show n
showLex (Lex _ _ (LexFloatAtom f)) = show f
showLex (Lex _ _ LexQuote) = "'"
showLex (Lex _ _ LexPoint) = "."
showLex (Lex _ _ LexAdd) = "+"
showLex (Lex _ _ LexSub) = "-"
showLex (Lex _ _ LexDiv) = "/"
showLex (Lex _ _ LexMul) = "*"
showLex (Lex _ _ LexEq) = "="
showLex (Lex _ _ LexGrt) = ">"
showLex (Lex _ _ LexGoE) = ">="
showLex (Lex _ _ LexLss) = "<"
showLex (Lex _ _ LexLoE) = "<="
showLex (Lex _ _ LexEof) = "<EOF>"

showSExpr :: SExpr -> String
showSExpr (SAtom lexTok) = showLex lexTok
showSExpr (SNil lexTok) = showLex lexTok
showSExpr (SList lst) =
    "(" ++ intercalate " " (map showSExpr (takeL lst 0 (-2))) ++ ")"

replaceLeafs :: ExprTree SExpr -> String -> ExprTree SExpr -> ExprTree SExpr
replaceLeafs (Node (x:xl)) str2 leaf =
    Node (replaceLeafs x str2 leaf : map (\tree -> replaceLeafs tree str2 leaf) xl)
replaceLeafs (Leaf (SAtom (Lex n m (LexAtom str1)))) str2 leaf
    | str1 == str2 = leaf
    | otherwise = Leaf (SAtom (Lex n m (LexAtom str1)))
replaceLeafs leafOld _ _ = leafOld

replaceLeafLst :: ExprTree SExpr -> [String] -> [ExprTree SExpr] -> ExprTree SExpr
replaceLeafLst tree [] [] = tree
replaceLeafLst tree (pname:pnames) (pvalue:pvalues) =
    replaceLeafLst (replaceLeafs tree pname pvalue) pnames pvalues
replaceLeafLst tree _ _ = tree

flattenTree :: ExprTree SExpr -> Either String (ExprTree SExpr)
flattenTree (Leaf sexpr) = Right (Leaf sexpr)
flattenTree (Node lst) =
    case flattenTreeHelp (map flattenTree lst) of
        Left err -> Left err
        Right sexprs -> Right (Leaf (SList sexprs))

flattenTreeHelp :: [Either String (ExprTree SExpr)] -> Either String [SExpr]
flattenTreeHelp [] = Right [SNil (Lex (-1) (-1) LexNil)]
flattenTreeHelp ((Right (Leaf sexpr)):xl) =
    case flattenTreeHelp xl of
        Left err -> Left err
        Right rest -> Right (sexpr : rest)
flattenTreeHelp ((Left err):_) = Left err
flattenTreeHelp _ = Left "Error: flattenTreeHelp: expected Leaf"
