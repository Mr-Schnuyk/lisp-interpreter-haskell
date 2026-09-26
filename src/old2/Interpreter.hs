module Interpreter (run, computeTree, computeArgs) where

import Types
import Utils
import Debug.Trace (trace)
import Control.Monad (sequence)

toFun :: ExprTree SExpr -> [Definition] -> Either String ([ExprTree SExpr] -> Either String (ExprTree SExpr))
toFun (Leaf (SAtom (Lex n m t))) def
    | t == LexAdd = Right (\args -> addLsp =<< computeArgs args def)
    | t == LexSub = Right (\args -> subLsp =<< computeArgs args def)
    | t == LexMul = Right (\args -> mulLsp =<< computeArgs args def)
    | t == LexDiv = Right (\args -> divLsp =<< computeArgs args def)
    | t == LexGrt = Right (\args -> grtLsp =<< computeArgs args def)
    | t == LexGoE = Right (\args -> goeLsp =<< computeArgs args def)
    | t == LexLss = Right (\args -> lssLsp =<< computeArgs args def)
    | t == LexLoE = Right (\args -> loeLsp =<< computeArgs args def)
    | t == LexEq = Right (\args -> equsLsp =<< computeArgs args def)
    | t == LexQuote = Right (\args -> quoteLsp args)
    | otherwise = atomToFun (Lex n m t) def
toFun (Leaf x) _ = Left $ "Error: " ++ (show x) ++ " is not a function name"
toFun _ _ = Left "Error: toFun"

atomToFun :: Lex -> [Definition] -> Either String ([ExprTree SExpr] -> Either String (ExprTree SExpr))
atomToFun (Lex n m (LexAtom str)) def
    | str == "QUOTE" = Right (\args -> quoteLsp args)
    | str == "EVAL" = Right (\args -> computeArgs args def >>= \computed -> evalLsp computed def)
    | str == "CAR" = Right (\args -> carLsp =<< computeArgs args def)
    | str == "CDR" = Right (\args -> cdrLsp =<< computeArgs args def)
    | str == "CONS" = Right (\args -> consLsp =<< computeArgs args def)
    | str == "COND" = Right (\args -> condLsp args def)
    | str == "EQ" = Right (\args -> eqLsp =<< computeArgs args def)
    | str == "EQL" = Right (\args -> eqlLsp =<< computeArgs args def)
    | str == "ATOM" = Right (\args -> atomLsp =<< computeArgs args def)
    | str == "AND" = Right (\args -> andLsp args def)
    | str == "OR" = Right (\args -> orLsp args def)
    | str == "NOT" = Right (\args -> notLsp =<< computeArgs args def)
    | str == "PRINT" = Right (\args -> printLsp =<< computeArgs args def)
    | str == "NULL" = Right (\args -> nullLsp =<< computeArgs args def)
    | str == "CAAR" = Right (\args -> caarLsp =<< computeArgs args def)
    | str == "CADR" = Right (\args -> cadrLsp =<< computeArgs args def)
    | str == "CDDR" = Right (\args -> cddrLsp =<< computeArgs args def)
    | str == "CDDDR" = Right (\args -> cdddrLsp =<< computeArgs args def)
    | str == "CADAR" = Right (\args -> cadarLsp =<< computeArgs args def)
    | str == "APPEND" = Right (\args -> appendLsp =<< computeArgs args def)
    | str == "MEMBER" = Right (\args -> memberLsp =<< computeArgs args def)
    | str == "INTERN" = Right (\args -> internLsp =<< computeArgs args def)
    | otherwise = 
        case defunLsp (Lex n m (LexAtom str)) def of
            Left err -> Left err
            Right f -> Right (\args -> f =<< computeArgs args def)
atomToFun x _ = Left $ "Error: " ++ (show x) ++ " is not a function name"

computeExpr :: ExprTree SExpr -> [Definition] -> Either String (ExprTree SExpr)
computeExpr (Leaf sexpr) _ = Right (Leaf sexpr)
computeExpr (Node []) _ = Left "computeExpr: empty node"
computeExpr (Node (x:xl)) def =
    toFun x def >>= \fun ->
    fun xl >>= \res -> Right (checkNil res)

checkNil :: ExprTree SExpr -> ExprTree SExpr
checkNil (Leaf (SList [(SNil lex)])) = (Leaf (SNil lex))
checkNil x = x

computeTree :: ExprTree SExpr -> [Definition] -> Either String (ExprTree SExpr)
computeTree (Leaf sexpr) def = computeExpr (Leaf sexpr) def
computeTree (Node (x:xl)) def =
    case computeTree x def of
        Left err -> Left err
        Right computedX -> computeExpr (Node (computedX : xl)) def

run :: [Definition] -> [Definition] -> Either String [Definition]
run [] _ = Right []
run ((Fun fun):xl) def =
    case run xl def of
        Left err -> Left err
        Right rest -> Right ((Fun fun) : rest)
run ((Expr expr):xl) def =
    case computeTree expr def of
        Left err -> Left err
        Right computed ->
            case run xl def of
                Left err -> Left err
                Right rest -> Right ((Expr computed) : rest)

computeArgs :: [ExprTree SExpr] -> [Definition] -> Either String [ExprTree SExpr]
computeArgs lst def = mapM (\tree -> computeTree tree def) lst

defunLsp :: Lex -> [Definition] -> Either String ([ExprTree SExpr] -> Either String (ExprTree SExpr))
defunLsp lex def = defunLspHelp lex def def

defunLspHelp :: Lex -> [Definition] -> [Definition] -> Either String ([ExprTree SExpr] -> Either String (ExprTree SExpr))
defunLspHelp (Lex n m (LexAtom str)) [] def = Left $ "Not find function '" ++ str ++ "' at " ++ show n ++ "::" ++ show m
defunLspHelp (Lex n m (LexAtom str)) ((Fun (UserFun name pnames body)):xl) def
    | str == name = return (\ treeLst -> aply (UserFun name pnames body) treeLst def)
    | otherwise = defunLspHelp (Lex n m (LexAtom str)) xl def
defunLspHelp lex (x:xl) def = defunLspHelp lex xl def

aply :: UserFun -> [ExprTree SExpr] -> [Definition] -> Either String (ExprTree SExpr)
aply (UserFun name pnames body) params def
    | length pnames /= length params = Left $ "Wrong number of arguments: '" ++ name ++ "' is expected to contain " ++ show (length pnames) ++ " argument."
    | otherwise =
        let computed = map (\treeExpr -> computeTree (replaceLeafLst treeExpr pnames params) def) body
        in case sequence computed of
               Left err -> Left err
               Right results -> deepSeqTree results

quoteLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
quoteLsp [x] = flattenTree x
quoteLsp _ = Left "Wrong number of arguments: 'quote' is expected to contain 1 argument."

evalLsp :: [ExprTree SExpr] -> [Definition] -> Either String (ExprTree SExpr)
evalLsp [(Leaf (SList lst))] def = computeTree (Node (makeLeafList lst)) def
evalLsp arg _ = Left ("Wrong number of arguments: 'eval' is expected to contain 1 argument." ++ show arg)

makeLeafList :: [SExpr] -> [ExprTree SExpr]
makeLeafList [] = []
makeLeafList [(SNil _)] = []
makeLeafList ((SList lst):xl) = (Node ((makeLeafList lst) ++ (makeLeafList xl))) : []
makeLeafList (x:xl) = (Leaf x) : makeLeafList xl

carLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
carLsp [(Leaf (SList (x:xl)))] = Right (Leaf x)
carLsp [(Leaf sexpr)] = Left ("Wrong type argument: It is expected that the 'car' " ++ (show sexpr) ++ " argument has the type 'list'.")
carLsp lst
    | allLeaf lst = Left "Wrong number of arguments: 'car' is expected to contain 1 argument."
    | otherwise = Left "Error: carLsp"

cdrLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
cdrLsp [(Leaf (SList [x]))] = Right (Leaf x)
cdrLsp [(Leaf (SList (x:xl)))] = Right (Leaf (SList xl))
cdrLsp [(Leaf _)] = Left "Wrong type argument: It is expected that the 'cdr' argument has the type 'list'."
cdrLsp lst
    | allLeaf lst = Left "Wrong number of arguments: 'cdr' is expected to contain 1 argument."
    | otherwise = Left "Error: cdrLsp"

consLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
consLsp ((Leaf sexpr):[(Leaf (SList lst))]) = Right (Leaf (SList (sexpr : lst)))
consLsp ((Leaf sexpr):[(Leaf (SNil lex))]) = Right (Leaf (SList (sexpr : [(SNil lex)])))
consLsp ((Leaf sexpr):[(Leaf _)]) = Left "Wrong type argument: It is expected that the 'cons' second argument has the type 'list'."
consLsp lst
    | allLeaf lst = Left "Wrong number of arguments: 'cons' is expected to contain 2 argument."
    | otherwise = Left "Error: consLsp"

condLsp :: [ExprTree SExpr] -> [Definition] -> Either String (ExprTree SExpr)
condLsp [] def = Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
condLsp ((Node (pred:[expr])):xl) def =
    case computeTree pred def of
        Left err -> Left err
        Right predVal ->
            if isEqExprTree (Leaf (SAtom (Lex (-1) (-1) LexT))) predVal
                then computeTree expr def
                else condLsp xl def
condLsp _ _ = Left "Error: uncorrect count of args for 'cond'"

eqLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
eqLsp (exprTree1:[exprTree2]) =
    if isEqExprTree exprTree1 exprTree2
        then Right (Leaf (SAtom (Lex (-1) (-1) LexT)))
        else Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
eqLsp _ = Left "Error: uncorrect count of args for 'eq'"

eqlLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
eqlLsp lst = eqLsp lst

atomLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
atomLsp [(Leaf (SList _))] = Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
atomLsp [x] = Right (Leaf (SAtom (Lex (-1) (-1) LexT)))
atomLsp _ = Left "Error: uncorrect count of args for 'atom'"

andLsp :: [ExprTree SExpr] -> [Definition] -> Either String (ExprTree SExpr)
andLsp [] def = Right (Leaf (SAtom (Lex (-1) (-1) LexT)))
andLsp (x:xl) def =
    case computeTree x def of
        Left err -> Left err
        Right xVal ->
            if isEqExprTree xVal (Leaf (SNil (Lex (-1) (-1) LexNil)))
                then Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
                else andLsp xl def

orLsp :: [ExprTree SExpr] -> [Definition] -> Either String (ExprTree SExpr)
orLsp [] def = Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
orLsp (x:xl) def =
    case computeTree x def of
        Left err -> Left err
        Right xVal ->
            if isEqExprTree xVal (Leaf (SNil (Lex (-1) (-1) LexNil)))
                then orLsp xl def
                else Right (Leaf (SAtom (Lex (-1) (-1) LexT)))

notLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
notLsp [(Leaf (SNil _))] = Right (Leaf (SAtom (Lex (-1) (-1) LexT)))
notLsp [x] = Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
notLsp _ = Left "Error: uncorrect count of args for 'not'"

equsLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
equsLsp (x:[y]) =
    case (isInt x, isInt y, isFloat x, isFloat y) of
        (Right True, Right True, _, _) -> do
            xi <- getInt x
            yi <- getInt y
            return $ if xi == yi then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (Right True, _, _, Right True) -> do
            xi <- getInt x
            yf <- getFloat y
            return $ if fromIntegral xi == yf then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (_, Right True, Right True, _) -> do
            xf <- getFloat x
            yi <- getInt y
            return $ if xf == fromIntegral yi then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (_, _, Right True, Right True) -> do
            xf <- getFloat x
            yf <- getFloat y
            return $ if xf == yf then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        _ -> Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
equsLsp _ = Left "Error: uncorrect count of args for '='"

addLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
addLsp (x:[y]) =
    case (isInt x, isInt y, isFloat x, isFloat y) of
        (Right True, Right True, _, _) -> do
            xi <- getInt x
            yi <- getInt y
            return (Leaf (SAtom (Lex (-1) (-1) (LexIntAtom (xi + yi)))))
        (Right True, _, _, Right True) -> do
            xi <- getInt x
            yf <- getFloat y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral xi + yf)))))
        (_, Right True, Right True, _) -> do
            xf <- getFloat x
            yi <- getInt y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf + fromIntegral yi)))))
        (_, _, Right True, Right True) -> do
            xf <- getFloat x
            yf <- getFloat y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf + yf)))))
        _ -> Left "Error: invalid arguments for '+'"
addLsp _ = Left "Error: uncorrect count of args for '+'"

subLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
subLsp [x] = subLspHelpI [x] (Leaf (SAtom (Lex (-1) (-1) (LexIntAtom 0))))
subLsp (x:xl) = subLspHelpI xl x
subLsp _ = Left "Error: uncorrect count of args for '-'"

subLspHelpI :: [ExprTree SExpr] -> ExprTree SExpr -> Either String (ExprTree SExpr)
subLspHelpI [y] x =
    case (isInt x, isInt y, isFloat x, isFloat y) of
        (Right True, Right True, _, _) -> do
            xi <- getInt x
            yi <- getInt y
            return (Leaf (SAtom (Lex (-1) (-1) (LexIntAtom (xi - yi)))))
        (Right True, _, _, Right True) -> do
            xi <- getInt x
            yf <- getFloat y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral xi - yf)))))
        (_, Right True, Right True, _) -> do
            xf <- getFloat x
            yi <- getInt y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf - fromIntegral yi)))))
        (_, _, Right True, Right True) -> do
            xf <- getFloat x
            yf <- getFloat y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf - yf)))))
        _ -> Left "Error: invalid arguments for '-'"
subLspHelpI (y:yl) x =
    case (isInt x, isInt y, isFloat x, isFloat y) of
        (Right True, Right True, _, _) -> do
            xi <- getInt x
            yi <- getInt y
            subLspHelpI yl (Leaf (SAtom (Lex (-1) (-1) (LexIntAtom (xi - yi)))))
        (Right True, _, _, Right True) -> do
            xi <- getInt x
            yf <- getFloat y
            subLspHelpF yl (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral xi - yf)))))
        (_, Right True, Right True, _) -> do
            xf <- getFloat x
            yi <- getInt y
            subLspHelpF yl (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf - fromIntegral yi)))))
        (_, _, Right True, Right True) -> do
            xf <- getFloat x
            yf <- getFloat y
            subLspHelpF yl (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf - yf)))))
        _ -> Left "Error: invalid arguments for '-'"
subLspHelpI _ _ = Left "Error: uncorrect count of args for '-'"

subLspHelpF :: [ExprTree SExpr] -> ExprTree SExpr -> Either String (ExprTree SExpr)
subLspHelpF [y] x =
    case (isFloat x, isInt y) of
        (Right True, Right True) -> do
            xf <- getFloat x
            yi <- getInt y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf - fromIntegral yi)))))
        (Right True, _) -> do
            xf <- getFloat x
            yf <- getFloat y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf - yf)))))
        _ -> Left "Error: invalid arguments for '-'"
subLspHelpF (y:yl) x =
    case (isFloat x, isInt y) of
        (Right True, Right True) -> do
            xf <- getFloat x
            yi <- getInt y
            subLspHelpF yl (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf - fromIntegral yi)))))
        (Right True, _) -> do
            xf <- getFloat x
            yf <- getFloat y
            subLspHelpF yl (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf - yf)))))
        _ -> Left "Error: invalid arguments for '-'"
subLspHelpF _ _ = Left "Error: uncorrect count of args for '-'"

mulLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
mulLsp (x:[y]) =
    case (isInt x, isInt y, isFloat x, isFloat y) of
        (Right True, Right True, _, _) -> do
            xi <- getInt x
            yi <- getInt y
            return (Leaf (SAtom (Lex (-1) (-1) (LexIntAtom (xi * yi)))))
        (Right True, _, _, Right True) -> do
            xi <- getInt x
            yf <- getFloat y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral xi * yf)))))
        (_, Right True, Right True, _) -> do
            xf <- getFloat x
            yi <- getInt y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf * fromIntegral yi)))))
        (_, _, Right True, Right True) -> do
            xf <- getFloat x
            yf <- getFloat y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf * yf)))))
        _ -> Left "Error: invalid arguments for '*'"
mulLsp _ = Left "Error: uncorrect count of args for '*'"

divLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
divLsp (x:[y]) =
    case (isInt x, isInt y, isFloat x, isFloat y) of
        (Right True, Right True, _, _) -> do
            xi <- getInt x
            yi <- getInt y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral xi / fromIntegral yi)))))
        (Right True, _, _, Right True) -> do
            xi <- getInt x
            yf <- getFloat y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral xi / yf)))))
        (_, Right True, Right True, _) -> do
            xf <- getFloat x
            yi <- getInt y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf / fromIntegral yi)))))
        (_, _, Right True, Right True) -> do
            xf <- getFloat x
            yf <- getFloat y
            return (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (xf / yf)))))
        _ -> Left "Error: invalid arguments for '/'"
divLsp _ = Left "Error: uncorrect count of args for '/'"

grtLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
grtLsp (x:[y]) =
    case (isInt x, isInt y, isFloat x, isFloat y) of
        (Right True, Right True, _, _) -> do
            xi <- getInt x
            yi <- getInt y
            return $ if xi > yi then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (Right True, _, _, Right True) -> do
            xi <- getInt x
            yf <- getFloat y
            return $ if fromIntegral xi > yf then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (_, Right True, Right True, _) -> do
            xf <- getFloat x
            yi <- getInt y
            return $ if xf > fromIntegral yi then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (_, _, Right True, Right True) -> do
            xf <- getFloat x
            yf <- getFloat y
            return $ if xf > yf then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        _ -> Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
grtLsp _ = Left "Error: uncorrect count of args for '>'"

lssLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
lssLsp (x:[y]) =
    case (isInt x, isInt y, isFloat x, isFloat y) of
        (Right True, Right True, _, _) -> do
            xi <- getInt x
            yi <- getInt y
            return $ if xi < yi then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (Right True, _, _, Right True) -> do
            xi <- getInt x
            yf <- getFloat y
            return $ if fromIntegral xi < yf then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (_, Right True, Right True, _) -> do
            xf <- getFloat x
            yi <- getInt y
            return $ if xf < fromIntegral yi then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (_, _, Right True, Right True) -> do
            xf <- getFloat x
            yf <- getFloat y
            return $ if xf < yf then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        _ -> Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
lssLsp _ = Left "Error: uncorrect count of args for '<'"

goeLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
goeLsp (x:[y]) =
    case (isInt x, isInt y, isFloat x, isFloat y) of
        (Right True, Right True, _, _) -> do
            xi <- getInt x
            yi <- getInt y
            return $ if xi >= yi then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (Right True, _, _, Right True) -> do
            xi <- getInt x
            yf <- getFloat y
            return $ if fromIntegral xi >= yf then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (_, Right True, Right True, _) -> do
            xf <- getFloat x
            yi <- getInt y
            return $ if xf >= fromIntegral yi then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (_, _, Right True, Right True) -> do
            xf <- getFloat x
            yf <- getFloat y
            return $ if xf >= yf then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        _ -> Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
goeLsp _ = Left "Error: uncorrect count of args for '>='"

loeLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
loeLsp (x:[y]) =
    case (isInt x, isInt y, isFloat x, isFloat y) of
        (Right True, Right True, _, _) -> do
            xi <- getInt x
            yi <- getInt y
            return $ if xi <= yi then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (Right True, _, _, Right True) -> do
            xi <- getInt x
            yf <- getFloat y
            return $ if fromIntegral xi <= yf then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (_, Right True, Right True, _) -> do
            xf <- getFloat x
            yi <- getInt y
            return $ if xf <= fromIntegral yi then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        (_, _, Right True, Right True) -> do
            xf <- getFloat x
            yf <- getFloat y
            return $ if xf <= yf then Leaf (SAtom (Lex (-1) (-1) LexT)) else Leaf (SNil (Lex (-1) (-1) LexNil))
        _ -> Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
loeLsp _ = Left "Error: uncorrect count of args for '<='"

printLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
printLsp [(Leaf sexpr)] = trace (showSExpr sexpr) (Right (Leaf sexpr))
printLsp lst
    | allLeaf lst = Left "Wrong number of arguments: 'print' is expected to contain 1 argument."
    | otherwise = Left ("Error: printLsp" ++ (show lst))

nullLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
nullLsp [x] = eqlLsp (x:[(Leaf (SNil (Lex (-1) (-1) LexNil)))])
nullLsp _ = Left "Error: uncorrect count of args for 'null'"

caarLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
caarLsp lst =
    case carLsp lst of
        Left err -> Left err
        Right carResult -> carLsp [carResult]

cadrLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
cadrLsp lst =
    case cdrLsp lst of
        Left err -> Left err
        Right cdrResult -> carLsp [cdrResult]

cddrLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
cddrLsp lst =
    case cdrLsp lst of
        Left err -> Left err
        Right cdrResult -> cdrLsp [cdrResult]

cdddrLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
cdddrLsp lst =
    case cdrLsp lst of
        Left err -> Left err
        Right cdr1 ->
            case cdrLsp [cdr1] of
                Left err -> Left err
                Right cdr2 -> cdrLsp [cdr2]

cadarLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
cadarLsp lst =
    case carLsp lst of
        Left err -> Left err
        Right carResult ->
            case cdrLsp [carResult] of
                Left err -> Left err
                Right cdrResult -> carLsp [cdrResult]

appendLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
appendLsp ((Leaf (SNil _)):[(Leaf (SList lst2))]) = Right (Leaf (SList lst2))
appendLsp ((Leaf (SList lst1)):[(Leaf (SList lst2))]) = Right (Leaf (SList ((takeL lst1 0 (-2)) ++ lst2)))
appendLsp ((Leaf (SList lst1)):[(Leaf (SNil _))]) = Right (Leaf (SList lst1))
appendLsp (x:xs:xl) = Left ("Wrong number of arguments: 'append' is expected to contain 2 argument.")
appendLsp _ = Left "Wrong type of arguments: It is expected that the 'append' arguments have the type 'list'."

memberLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
memberLsp ((Leaf expr):[(Leaf (SNil _))]) = Right (Leaf (SNil (Lex (-1) (-1) LexNil)))
memberLsp ((Leaf expr):[(Leaf (SList (x:xl)))])
    | isEqExpr x expr = Right (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | otherwise = memberLsp ((Leaf expr) : (checkNil (Leaf (SList (xl)))) : [])
memberLsp (x:xs:xl) = Left ("Wrong number of arguments: 'member' is expected to contain 2 argument.")
memberLsp _ = Left "Wrong type of arguments: It is expected that the 'member' arguments have the type 'list'."

internLsp :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
internLsp ((Leaf (SAtom (Lex _ _ (LexAtom str1)))):[(Leaf (SAtom (Lex _ _ (LexAtom str2))))]) =
    Right (Leaf (SAtom (Lex (-1) (-1) (LexAtom (str1 ++ str2)))))
internLsp ((Leaf (SAtom (Lex _ _ (LexAtom str1)))):[(Leaf (SAtom (Lex _ _ (LexIntAtom num))))]) =
    Right (Leaf (SAtom (Lex (-1) (-1) (LexAtom (str1 ++ show num)))))
internLsp (x:xs:xl) = Left ("Wrong number of arguments: 'intern' is expected to contain 2 argument." ++ show xs ++ show xl)
internLsp _ = Left "Wrong type of arguments: It is expected that the 'intern' arguments have the type 'list'."