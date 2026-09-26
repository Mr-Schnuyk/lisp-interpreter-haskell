module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile, hFlush, stdout)
import Debug.Trace

import Types (Definition(..), ExprTree(..), SExpr(..), Lex(..), LexType(..))
import Lexer (parseStart)
import Parser (sntxAnlz)
import Semantic (smtcAnlz)
import Interpreter (run)
import Utils (deepSeqTree)

takeExprs :: [Definition] -> [ExprTree SExpr]
takeExprs [] = [(Leaf (SAtom (Lex (-1) (-1) LexEof)))]
takeExprs ((Expr expr):xl) = expr : takeExprs xl
takeExprs ((Fun _):xl) = takeExprs xl

takeFuns :: [Definition] -> [Definition]
takeFuns [] = []
takeFuns ((Expr _):xl) = takeFuns xl
takeFuns ((Fun fun):xl) = (Fun fun) : takeFuns xl

repl :: [Definition] -> IO ()
repl defFuns = do
    putStr ">>>"
    hFlush stdout
    pgm <- getLine
    if pgm == "exit"
        then putStrLn "End of interpretation"
        else do
            case parseStart pgm of
                Left err -> putStrLn ("Lexer error: " ++ err) >> repl defFuns
                Right tokens ->
                    case sntxAnlz tokens of
                        Left err -> putStrLn ("Parser error: " ++ err) >> repl defFuns
                        Right exprTrees ->
                            case smtcAnlz exprTrees of
                                Left err -> putStrLn ("Semantic error: " ++ err) >> repl defFuns
                                Right def ->
                                    case run def def of
                                        Left err -> putStrLn ("Interpreter error: " ++ err) >> repl defFuns
                                        Right result ->
                                            case deepSeqTree (takeExprs result) of
                                                Left err -> putStrLn ("Evaluation error: " ++ err) >> repl defFuns
                                                Right _ -> repl (takeFuns result)

processProgram :: String -> IO ()
processProgram pgm = do
    case parseStart pgm of
        Left err -> putStrLn ("Lexer error: " ++ err)
        Right tokens ->
            case sntxAnlz tokens of
                Left err -> putStrLn ("Parser error: " ++ err)
                Right exprTrees ->
                    case smtcAnlz exprTrees of
                        Left err -> putStrLn ("Semantic error: " ++ err)
                        Right def ->
                            case run def def of
                                Left err -> putStrLn ("Interpreter error: " ++ err)
                                Right results ->
                                    case deepSeqTree (takeExprs results) of
                                        Left err -> putStrLn ("Evaluation error: " ++ err)
                                        Right _ -> putStrLn "Compilation is successful"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            text <- readFile filename
            processProgram text
        [] -> do
            putStrLn "---Interpretation mode---"
            putStrLn "Enter program (for end enter 'exit'):"
            putStrLn "-------------------------"
            repl []
        _ -> putStrLn "Usage: .\\lisp-exe [filename]"