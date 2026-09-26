module Lexer (parseStart) where

import Types
import Data.Char (isSpace, isAlpha, isDigit, toUpper)
import Data.List (intercalate)

parseStart :: String -> Either String [Lex]
parseStart str = parse str 0 0

parse :: String -> Int -> Int -> Either String [Lex]
parse [] line col = Right [(Lex line col LexEof)]
parse (x:xs:xl) line col
    | x == '>' && xs == '=' =
        (Lex line col LexGoE :) <$> parse xl line (col + 2)
    | x == '<' && xs == '=' =
        (Lex line col LexLoE :) <$> parse xl line (col + 2)
    | x == '(' && xs == ')' =
        (Lex line col LexNil :) <$> parse xl line (col + 2)
    | otherwise = parseHelp (x:xs:xl) line col
parse str line col = parseHelp str line col

parseHelp :: String -> Int -> Int -> Either String [Lex]
parseHelp [] line col = Right [(Lex line col LexEof)]
parseHelp (x:xl) line col
    | x == '\n' = parse xl (line + 1) 0
    | isSpace x = parse xl line (col + 1)
    | x == ';' = parseComment (x:xl) line col
    | isAlpha x || x == '_' =
        parseAtom (x:xl) [] line col col
    | isDigit x =
        parseDigit (x:xl) [] line col col
    | x == '"' =
        parseString xl [] line (col + 1) col
    | x == '(' =
        (Lex line col LexOP :) <$> parse xl line (col + 1)
    | x == ')' =
        (Lex line col LexCP :) <$> parse xl line (col + 1)
    | x == '\'' =
        (Lex line col LexQuote :) <$> parse xl line (col + 1)
    | x == '.' =
        (Lex line col LexPoint :) <$> parse xl line (col + 1)
    | x == '+' =
        (Lex line col LexAdd :) <$> parse xl line (col + 1)
    | x == '-' =
        (Lex line col LexSub :) <$> parse xl line (col + 1)
    | x == '/' =
        (Lex line col LexDiv :) <$> parse xl line (col + 1)
    | x == '*' =
        (Lex line col LexMul :) <$> parse xl line (col + 1)
    | x == '=' =
        (Lex line col LexEq :) <$> parse xl line (col + 1)
    | x == '>' =
        (Lex line col LexGrt :) <$> parse xl line (col + 1)
    | x == '<' =
        (Lex line col LexLss :) <$> parse xl line (col + 1)
    | otherwise =
        Left $ "Unexpected symbol: " ++ [x] ++ " at line "
                ++ show line ++ ", col " ++ show col

parseComment :: String -> Int -> Int -> Either String [Lex]
parseComment [] line col = Right [(Lex line col LexEof)]
parseComment (x:xl) line col
    | x /= '\n' = parseComment xl line (col + 1)
    | otherwise = parse xl (line + 1) 0

parseAtom :: String -> String -> Int -> Int -> Int -> Either String [Lex]
parseAtom [] [] line col _ =
    Right [(Lex line col LexEof)]
parseAtom [] atom line col firstCol =
    Right [Lex line firstCol (parseBool atom), Lex line col LexEof]
parseAtom (x:xl) atom line col firstCol
    | isAlpha x || isDigit x || x == '_' =
        parseAtom xl (atom ++ [x]) line (col + 1) firstCol
    | otherwise =
        (Lex line firstCol (parseBool atom) :) <$> parse (x:xl) line col

parseBool :: String -> LexType
parseBool atom
    | map toUpper atom == "T" = LexT
    | map toUpper atom == "NIL" = LexNil
    | otherwise = LexAtom (map toUpper atom)

parseString :: String -> String -> Int -> Int -> Int -> Either String [Lex]
parseString [] atom line _ firstCol =
    Left $ "Unterminated string: " ++ atom
            ++ " starting at line " ++ show line
            ++ ", col " ++ show firstCol
parseString (x:xl) atom line col firstCol
    | x /= '"' =
        parseString xl (atom ++ [x]) line (col + 1) firstCol
    | otherwise =
        (Lex line firstCol (LexStringAtom atom) :)
            <$> parse xl line (col + 1)

parseDigit :: String -> String -> Int -> Int -> Int -> Either String [Lex]
parseDigit [] [] line col _ =
    Right [(Lex line col LexEof)]
parseDigit [] dig line col firstCol =
    Right [Lex line firstCol (LexIntAtom (read dig)), Lex line col LexEof]
parseDigit (x:xl) dig line col firstCol
    | isDigit x =
        parseDigit xl (dig ++ [x]) line (col + 1) firstCol
    | x == '.' =
        parseFloat xl (dig ++ [x]) line (col + 1) firstCol
    | otherwise =
        (Lex line firstCol (LexIntAtom (read dig)) :)
            <$> parse (x:xl) line col

parseFloat :: String -> String -> Int -> Int -> Int -> Either String [Lex]
parseFloat [] [] _ _ _ = Left "Unexpected parse error in float"
parseFloat [] dig line col firstCol
    | last dig /= '.' =
        Right [Lex line firstCol (LexFloatAtom (read dig)), Lex line col LexEof]
    | otherwise =
        Left "Parse error: Unterminated number (reached end of file)"
parseFloat (x:xl) dig line col firstCol
    | isDigit x =
        parseFloat xl (dig ++ [x]) line (col + 1) firstCol
    | last dig == '.' =
        Left $ "Parse error: Unterminated number (expected digit after dot)"
                ++ " at line " ++ show line ++ ", col " ++ show firstCol
    | otherwise =
        (Lex line firstCol (LexFloatAtom (read dig)) :)
            <$> parse (x:xl) line col