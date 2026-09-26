module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile, hFlush, stdout)
import Control.Exception (catch, evaluate, ErrorCall(ErrorCall))
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
            let def = (smtcAnlz $sntxAnlz $ parseStart pgm) ++ defFuns
            result <- evaluate $ run def def
            _ <- evaluate (deepSeqTree (takeExprs result))
            repl (takeFuns result)

processProgram :: String -> IO ()
processProgram pgm = do
    let definitions = smtcAnlz $ sntxAnlz $ parseStart pgm
    let results = traceShowId $run definitions definitions
    _ <- evaluate $ (deepSeqTree $ takeExprs results) 
    putStrLn "Compilation is successful"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> 
            catch (do
                text <- readFile filename
                processProgram text
            ) handleError
        [] -> 
            catch (do
                putStrLn "---Interpretation mode---"
                putStrLn "Enter program (for end enter 'exit'):"
                putStrLn "-------------------------"
                repl []
            ) handleError
        _ -> putStrLn "Usage: .\\lisp-exe [filename]"
  where
    handleError :: ErrorCall -> IO ()
    handleError (ErrorCall msg) = do
        putStrLn $ show msg
	
	