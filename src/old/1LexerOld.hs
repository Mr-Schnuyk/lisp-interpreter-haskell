module Lexer (parseStart, printTokens) where

import Types
import Data.Char (isSpace, isAlpha, isDigit, toUpper)
import Data.List (intercalate)
import Debug.Trace (trace)

-- Кастомная печать: каждая лексема на новой строке
printTokens :: [Lex] -> [Lex]
printTokens tokens = trace (intercalate "\n" ((map show tokens) ++ [""])) tokens

parseStart :: String -> [Lex]
parseStart str = parse str 0 0

parse :: String -> Int -> Int -> [Lex]
parse [] posSTR posSYM = [(Lex posSTR posSYM LexEof)]
parse (x:xs:xl) posSTR posSYM
    | x == '>' && xs == '=' = (Lex posSTR posSYM LexGoE) : parse xl posSTR (posSYM + 2)
    | x == '<' && xs == '=' = (Lex posSTR posSYM LexLoE) : parse xl posSTR (posSYM + 2)
    | x == '(' && xs == ')' = (Lex posSTR posSYM LexNil) : parse xl posSTR (posSYM + 2)
    | otherwise = parseHelp (x:xs:xl) posSTR posSYM        
parse str posSTR posSYM = parseHelp str posSTR posSYM

parseHelp :: String -> Int -> Int -> [Lex]
parseHelp [] posSTR posSYM = [(Lex posSTR posSYM LexEof)]
parseHelp (x:xl) posSTR posSYM
    | x == '\n' = parse xl (posSTR + 1) 0
    | isSpace x = parse xl posSTR (posSYM + 1)
    | x == ';' = parseComment (x:xl) posSTR posSYM
    | isAlpha x || x == '_' = parseAtom (x:xl) [] posSTR posSYM posSYM
    | isDigit x = parseDigit (x:xl) [] posSTR posSYM posSYM
    | x == '"' = parseString xl [] posSTR (posSYM + 1) posSYM
    | x == '(' = (Lex posSTR posSYM LexOP) : parse xl posSTR (posSYM + 1)
    | x == ')' = (Lex posSTR posSYM LexCP) : parse xl posSTR (posSYM + 1)
    | x == '\'' = (Lex posSTR posSYM LexQuote) : parse xl posSTR (posSYM + 1)
    | x == '.' = (Lex posSTR posSYM LexPoint) : parse xl posSTR (posSYM + 1)
    | x == '+' = (Lex posSTR posSYM LexAdd) : parse xl posSTR (posSYM + 1)
    | x == '-' = (Lex posSTR posSYM LexSub) : parse xl posSTR (posSYM + 1)
    | x == '/' = (Lex posSTR posSYM LexDiv) : parse xl posSTR (posSYM + 1)
    | x == '*' = (Lex posSTR posSYM LexMul) : parse xl posSTR (posSYM + 1)
    | x == '=' = (Lex posSTR posSYM LexEq) : parse xl posSTR (posSYM + 1)
    | x == '>' = (Lex posSTR posSYM LexGrt) : parse xl posSTR (posSYM + 1)
    | x == '<' = (Lex posSTR posSYM LexLss) : parse xl posSTR (posSYM + 1)
    | otherwise = error $ "Unexpected symbol: " ++ [x]

parseComment :: String -> Int -> Int -> [Lex]
parseComment [] posSTR posSYM = [(Lex posSTR posSYM LexEof)]
parseComment (x:xl) posSTR posSYM 
    | x /= '\n' = parseComment xl posSTR (posSYM + 1)
    | otherwise = parse xl (posSTR + 1) 0

parseAtom :: String -> String -> Int -> Int -> Int -> [Lex]
parseAtom [] [] posSTR posSYM fposSYM = [(Lex posSTR posSYM LexEof)]
parseAtom [] atom posSTR posSYM fposSYM = (Lex posSTR fposSYM (parseBool atom)) : [(Lex posSTR posSYM LexEof)]
parseAtom (x:xl) atom posSTR posSYM fposSYM
    | isAlpha x || isDigit x || x == '_' = parseAtom xl (atom ++ [x]) posSTR (posSYM + 1) fposSYM
    | otherwise = (Lex posSTR fposSYM (parseBool atom)) : parse (x:xl) posSTR posSYM

parseBool :: String -> LexType
parseBool atom
    | (map toUpper atom) == "T" = LexT
    | (map toUpper atom) == "NIL" = LexNil
    | otherwise = (LexAtom (map toUpper atom))

parseString :: String -> String -> Int -> Int -> Int -> [Lex]
parseString [] atom posSTR posSYM fposSYM = error $ "Unterminated string: " ++ atom ++ " (reached end of file)"
parseString (x:xl) atom posSTR posSYM fposSYM
    | x /= '"' = parseString xl (atom ++ [x]) posSTR (posSYM + 1) fposSYM
    | otherwise = (Lex posSTR fposSYM (LexStringAtom atom)) : parse xl posSTR (posSYM + 1)

parseDigit :: String -> String -> Int -> Int -> Int -> [Lex]
parseDigit [] [] posSTR posSYM fposSYM = [(Lex posSTR posSYM LexEof)]
parseDigit [] dig posSTR posSYM fposSYM = (Lex posSTR fposSYM (LexIntAtom (read dig))) : [(Lex posSTR posSYM LexEof)]
parseDigit (x:xl) dig posSTR posSYM fposSYM
    | isDigit x = parseDigit xl (dig ++ [x]) posSTR (posSYM + 1) fposSYM
    | x == '.' = parseFloat xl (dig ++ [x]) posSTR (posSYM + 1) fposSYM
    | otherwise = (Lex posSTR fposSYM (LexIntAtom (read dig))) : parse (x:xl) posSTR posSYM

parseFloat :: String -> String -> Int -> Int -> Int -> [Lex]
parseFloat [] [] posSTR posSYM fposSYM = error "Unexpected parse error"
parseFloat [] dig posSTR posSYM fposSYM
    | last dig /= '.' = (Lex posSTR fposSYM (LexFloatAtom (read dig))) : [(Lex posSTR posSYM LexEof)]
    | otherwise = error "Parse error: Unterminated number (reached end of file)"
parseFloat (x:xl) dig posSTR posSYM fposSYM
    | isDigit x = parseFloat xl (dig ++ [x]) posSTR (posSYM + 1) fposSYM
    | last dig == '.' = error "Parse error: Unterminated number"
    | otherwise = (Lex posSTR fposSYM (LexFloatAtom (read dig))) : parse (x:xl) posSTR posSYM