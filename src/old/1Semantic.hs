module Semantic (smtcAnlz) where

import Types

smtcAnlz :: [ExprTree SExpr] -> [Definition]
smtcAnlz exptrLst = checkFun $ smtcAnlzHelp exptrLst

smtcAnlzHelp :: [ExprTree SExpr] -> [Definition]
smtcAnlzHelp [] = []
smtcAnlzHelp ((Node ((Leaf (SAtom (Lex _ _ (LexAtom "DEFUN")))):(Leaf (SAtom (Lex _ _ (LexAtom name)))):(Node params):yl)):xl)
    | length yl > 0 = (Fun (UserFun name (getParamsNames params) yl)) : smtcAnlzHelp xl
smtcAnlzHelp ((Node ((Leaf (SAtom (Lex n m (LexAtom "DEFUN")))):xl)):yl) = error ("Invalid defun at " ++ show n ++ "::" ++ show m)
smtcAnlzHelp (x:xl) = (Expr x) : smtcAnlzHelp xl

getParamsNames :: [ExprTree SExpr] -> [String]
getParamsNames [] = []
getParamsNames ((Leaf (SAtom (Lex _ _ (LexAtom name)))):xl) = name : getParamsNames xl
getParamsNames _ = error ("Defun error: invalid params")

checkFun :: [Definition] -> [Definition]
checkFun [] = []
checkFun ((Fun (UserFun name nparams body)):xl)
    | isFun xl name = error ("Double defun a function '" ++ name ++ "'")
    | otherwise = (Fun (UserFun name nparams body)) : checkFun xl
checkFun (x:xl) = x : checkFun xl

isFun :: [Definition] -> String -> Bool
isFun [] _ = False
isFun ((Fun (UserFun name1 _ _)):xl) name2
    | name1 == name2 = True
    | otherwise = isFun xl name2
isFun (x:xl) name2 = isFun xl name2