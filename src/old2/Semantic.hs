module Semantic (smtcAnlz) where

import Types

smtcAnlz :: [ExprTree SExpr] -> Either String [Definition]
smtcAnlz exptrLst =
    case smtcAnlzHelp exptrLst of
        Left err -> Left err
        Right defs -> checkFun defs

smtcAnlzHelp :: [ExprTree SExpr] -> Either String [Definition]
smtcAnlzHelp [] = Right []
smtcAnlzHelp ((Node ((Leaf (SAtom (Lex _ _ (LexAtom "DEFUN")))):(Leaf (SAtom (Lex _ _ (LexAtom name)))):(Node params):yl)):xl)
    | length yl > 0 =
        case getParamsNames params of
            Left err -> Left err
            Right pnames ->
                case smtcAnlzHelp xl of
                    Left err -> Left err
                    Right rest -> Right ((Fun (UserFun name pnames yl)) : rest)
smtcAnlzHelp ((Node ((Leaf (SAtom (Lex n m (LexAtom "DEFUN")))):xl)):yl) =
    Left $ "Invalid defun at " ++ show n ++ "::" ++ show m
smtcAnlzHelp (x:xl) =
    case smtcAnlzHelp xl of
        Left err -> Left err
        Right rest -> Right ((Expr x) : rest)

getParamsNames :: [ExprTree SExpr] -> Either String [String]
getParamsNames [] = Right []
getParamsNames ((Leaf (SAtom (Lex _ _ (LexAtom name)))):xl) =
    case getParamsNames xl of
        Left err -> Left err
        Right rest -> Right (name : rest)
getParamsNames _ = Left "Defun error: invalid params"

checkFun :: [Definition] -> Either String [Definition]
checkFun [] = Right []
checkFun ((Fun (UserFun name nparams body)):xl) =
    case isFun xl name of
        Left err -> Left err
        Right True -> Left $ "Double defun a function '" ++ name ++ "'"
        Right False ->
            case checkFun xl of
                Left err -> Left err
                Right rest -> Right ((Fun (UserFun name nparams body)) : rest)
checkFun (x:xl) =
    case checkFun xl of
        Left err -> Left err
        Right rest -> Right (x : rest)

isFun :: [Definition] -> String -> Either String Bool
isFun [] _ = Right False
isFun ((Fun (UserFun name1 _ _)):xl) name2
    | name1 == name2 = Right True
    | otherwise = isFun xl name2
isFun (x:xl) name2 = isFun xl name2