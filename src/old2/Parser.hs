module Parser (sntxAnlz) where

import Types
import Utils (getEl, takeL, eqLexTypes, getStr, getSym)

sntxAnlz :: [Lex] -> Either String [ExprTree SExpr]
sntxAnlz lst =
    case getEl lst (-1) of
        Left err -> Left err
        Right lastLex ->
            if eqLexTypes lastLex LexEof
                then sntxAnlzHelp (takeL lst 0 (-2))
                else Left "Syntax error: Expected eof"

sntxAnlzHelp :: [Lex] -> Either String [ExprTree SExpr]
sntxAnlzHelp [] = Right []
sntxAnlzHelp lst =
    case takeList lst of
        [] -> Left "Parser error: takeList returned empty"
        taken ->
            case rdS taken of
                Left err -> Left err
                Right s ->
                    case sntxAnlzHelp (skipList lst) of
                        Left err -> Left err
                        Right rest -> Right (s : rest)

rdS :: [Lex] -> Either String (ExprTree SExpr)
rdS [] = Left "Syntax error: Expected at least 1 list"
rdS lst =
    case (getEl lst 0, getEl lst (-1)) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right first, Right last) ->
            if eqLexTypes first LexOP && eqLexTypes last LexCP
                then do
                    let inner = takeL lst 1 (-2)
                    a <- rdA (takeList inner)
                    e <- rdE (skipList inner)
                    Right (Node (a : e))
                else if eqLexTypes first LexOP
                    then case getEl lst (-1) of
                        Left err -> Left err
                        Right lastLex ->
                            Left $ "Syntax error: Expected ')' at " ++
                            (show $ getStr lastLex) ++ "::" ++ (show $ getSym lastLex)
                    else case getEl lst 0 of
                        Left err -> Left err
                        Right firstLex ->
                            Left $ "Syntax error: Expected '(' at " ++
                            (show $ getStr firstLex) ++ "::" ++ (show $ getSym firstLex)

rdE :: [Lex] -> Either String [ExprTree SExpr]
rdE [] = Right []
rdE (x:xl)
    | eqLexTypes x LexQuote =
        let quoted = (Lex (-1) (-1) LexOP) : (x : takeList xl ++ [(Lex (-1) (-1) LexCP)])
        in case rdA quoted of
               Left err -> Left err
               Right a ->
                   case rdE (skipList xl) of
                       Left err -> Left err
                       Right rest -> Right (a : rest)
    | otherwise =
        case rdA (takeList (x:xl)) of
            Left err -> Left err
            Right a ->
                case rdE (skipList (x:xl)) of
                    Left err -> Left err
                    Right rest -> Right (a : rest)

rdA :: [Lex] -> Either String (ExprTree SExpr)
rdA [] = Left "Unexpected syntax error"
rdA (x:xl)
    | eqLexTypes x LexOP = rdS (x:xl)
    | eqLexTypes x LexNil = Right (Leaf (SNil x))
    | otherwise = Right (Leaf (SAtom x))

takeList :: [Lex] -> [Lex]
takeList [] = []
takeList (x:xl)
    | eqLexTypes x LexOP = takeListHelp xl [x] 1
    | otherwise = [x]

takeListHelp :: [Lex] -> [Lex] -> Int -> [Lex]
takeListHelp [] [] _ = []
takeListHelp [] lst _ = lst
takeListHelp (x:xl) lst n
    | n == 0 = lst
    | eqLexTypes x LexOP = takeListHelp xl (lst ++ [x]) (n + 1)
    | eqLexTypes x LexCP = takeListHelp xl (lst ++ [x]) (n - 1)
    | otherwise = takeListHelp xl (lst ++ [x]) n

skipList :: [Lex] -> [Lex]
skipList [] = []
skipList (x:xl)
    | eqLexTypes x LexOP = skipListHelp xl 1
    | otherwise = xl

skipListHelp :: [Lex] -> Int -> [Lex]
skipListHelp [] _ = []
skipListHelp (x:xl) n
    | n == 0 = (x:xl)
    | eqLexTypes x LexOP = skipListHelp xl (n + 1)
    | eqLexTypes x LexCP = skipListHelp xl (n - 1)
    | otherwise = skipListHelp xl n