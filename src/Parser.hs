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
                else
                    let line = getStr lastLex
                        col  = getSym lastLex
                    in Left $ "Syntax error: Expected eof at "
                               ++ show line ++ "::" ++ show col

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
        (Right first, Right lastTok) ->
            if eqLexTypes first LexOP && eqLexTypes lastTok LexCP
                then let inner = takeL lst 1 (-2)
                     in rdA (takeList inner) >>= \a ->
                        rdE (skipList inner) >>= \e ->
                        Right (Node (a : e))
                else if eqLexTypes first LexOP
                    then let line = getStr lastTok
                             col  = getSym lastTok
                         in Left $ "Syntax error: Expected ')' at "
                                   ++ show line ++ "::" ++ show col
                    else let line = getStr first
                             col  = getSym first
                         in Left $ "Syntax error: Expected '(' at "
                                   ++ show line ++ "::" ++ show col

rdE :: [Lex] -> Either String [ExprTree SExpr]
rdE [] = Right []
rdE (x:xl)
    | eqLexTypes x LexQuote =
        let quoted = (Lex (-1) (-1) LexOP) :
                     (x : takeList xl ++ [(Lex (-1) (-1) LexCP)])
        in rdA quoted >>= \a ->
           rdE (skipList xl) >>= \rest ->
           Right (a : rest)
    | otherwise =
        rdA (takeList (x:xl)) >>= \a ->
        rdE (skipList (x:xl)) >>= \rest ->
        Right (a : rest)

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