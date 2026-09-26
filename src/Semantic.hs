module Semantic (smtcAnlz) where

import Types

smtcAnlz :: [ExprTree SExpr] -> Either String [Definition]
smtcAnlz exptrLst =
    case smtcAnlzHelp exptrLst of
        Left err -> Left err
        Right defs -> checkFun defs

smtcAnlzHelp :: [ExprTree SExpr] -> Either String [Definition]
smtcAnlzHelp [] = Right []
smtcAnlzHelp ((Node ((Leaf (SAtom (Lex _ _ (LexAtom "DEFUN"))))
                     :(Leaf (SAtom (Lex _ _ (LexAtom funcName))))
                     :(Node params):yl)):xl)
    | length yl > 0 =
        case getParamsNames params of
            Left err -> Left err
            Right pnames ->
                case smtcAnlzHelp xl of
                    Left err -> Left err
                    Right rest -> Right ((Fun (UserFun funcName pnames yl)) : rest)
smtcAnlzHelp ((Node ((Leaf (SAtom (Lex n m (LexAtom "DEFUN")))):_)):_) =
    Left $ "Invalid defun at " ++ show n ++ "::" ++ show m
smtcAnlzHelp (x:xl) =
    case smtcAnlzHelp xl of
        Left err -> Left err
        Right rest -> Right ((Expr x) : rest)

getParamsNames :: [ExprTree SExpr] -> Either String [String]
getParamsNames [] = Right []
getParamsNames ((Leaf (SAtom (Lex _ _ (LexAtom paramName)))):xl) =
    case getParamsNames xl of
        Left err -> Left err
        Right rest -> Right (paramName : rest)
getParamsNames _ = Left "Defun error: invalid params"

checkFun :: [Definition] -> Either String [Definition]
checkFun [] = Right []
checkFun ((Fun (UserFun funcName params funcBody)):xl) =
    case isFun xl funcName of
        Left err -> Left err
        Right True -> Left $ "Double defun a function '" ++ funcName ++ "'"
        Right False ->
            case checkFun xl of
                Left err -> Left err
                Right rest -> Right ((Fun (UserFun funcName params funcBody)) : rest)
checkFun (x:xl) =
    case checkFun xl of
        Left err -> Left err
        Right rest -> Right (x : rest)

isFun :: [Definition] -> String -> Either String Bool
isFun [] _ = Right False
isFun ((Fun (UserFun name1 _ _)):xl) name2
    | name1 == name2 = Right True
    | otherwise = isFun xl name2
isFun (_:xl) name2 = isFun xl name2