module Parser (sntxAnlz) where

import Types
import Utils (getEl, takeL, eqLexTypes, getStr, getSym)
import Debug.Trace (trace)

--S → (A E1)
--E → A E | ε
--A → S | atom

sntxAnlz :: [Lex] -> [ExprTree SExpr]
sntxAnlz lst
    | eqLexTypes (getEl lst (-1)) LexEof = sntxAnlzHelp $ takeL lst 0 (-2)
    | otherwise = error "Syntax error: Expected eof"

-- sntxAnlzHelp :: [Lex] -> [ExprTree SExpr]
-- sntxAnlzHelp [] = []
-- sntxAnlzHelp lst = trace (show $ ((rdS $ takeList lst) : (sntxAnlzHelp $ skipList lst))) ((rdS $ takeList lst) : (sntxAnlzHelp $ skipList lst))

sntxAnlzHelp :: [Lex] -> [ExprTree SExpr]
sntxAnlzHelp [] = []
sntxAnlzHelp lst = ((rdS $ takeList lst) : (sntxAnlzHelp $ skipList lst))

rdS :: [Lex] -> ExprTree SExpr
rdS [] = error "Syntax error: Expected at least 1 list"
rdS lst
    | eqLexTypes (getEl lst 0) LexOP && eqLexTypes (getEl lst (-1)) LexCP = 
		(Node ((rdA $ takeList $ takeL lst 1 (-2)) : (rdE $ skipList $ takeL lst 1 (-2))))
    | eqLexTypes (getEl lst 0) LexOP = error ("Syntax error: Expected ')' at " ++ 
        (show $ getStr $ getEl lst (-1)) ++ "::" ++ (show $ getSym $ getEl lst (-1)))
    | otherwise = error ("Syntax error: Expected '(' at " ++ 
        (show $ getStr $ getEl lst 0) ++ "::" ++ (show $ getSym $ getEl lst 0))

rdE :: [Lex] -> [ExprTree SExpr]
rdE [] = []
rdE (x:xl)
    | eqLexTypes x LexQuote = (rdA $ (Lex (-1) (-1) LexOP) : (x : takeList xl ++ [(Lex (-1) (-1) LexCP)])) : (rdE $ skipList xl)
    | otherwise = (rdA $ takeList (x:xl)) : (rdE $ skipList (x:xl))

rdA :: [Lex] -> ExprTree SExpr
rdA [] = error "Unexpected syntax error"
rdA (x:xl)
    | eqLexTypes x LexOP = rdS (x:xl)
    | eqLexTypes x LexNil = (Leaf (SNil x))
    | otherwise = (Leaf (SAtom x))

takeList :: [Lex] -> [Lex]
takeList [] = []
takeList (x:xl)
    | eqLexTypes x LexOP = takeListHelp xl [x] 1 
    | otherwise = [x]

takeListHelp :: [Lex] -> [Lex] -> Int -> [Lex]
takeListHelp [] [] _ = []
takeListHelp [] lst _ = lst
takeListHelp (x:xl) lst n 
    | n == 0 =  lst
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
    | n == 0 =  (x:xl)
    | eqLexTypes x LexOP = skipListHelp xl (n + 1)
    | eqLexTypes x LexCP = skipListHelp xl (n - 1)
    | otherwise = skipListHelp xl n