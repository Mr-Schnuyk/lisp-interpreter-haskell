module Interpreter (run, computeTree, computeArgs) where

import Types
import Utils
import Debug.Trace (trace)

-- Алгоритм вычисления функции, заданной списком деревьев (x:xl) (первое дерево - функция):
-- 1) вычислить x
-- 2) вызвать функцию x c параметрами xl
-- 3) вычислить параметры (если необходимо)
-- 4) вычислить функцию

toFun :: ExprTree SExpr -> [Definition] -> ([ExprTree SExpr] -> ExprTree SExpr)
toFun (Leaf (SAtom (Lex n m t))) def
    | t == LexAdd = addLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | t == LexSub = subLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | t == LexMul = mulLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | t == LexDiv = divLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | t == LexGrt = grtLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | t == LexGoE = goeLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | t == LexLss = lssLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | t == LexLoE = loeLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | t == LexEq = equsLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | t == LexQuote = quoteLsp
    | otherwise = atomToFun (Lex n m t) def
toFun (Leaf x) _ = error ("Error: " ++ (show x) ++ " is not a function name")
toFun _ _ = error "Error: toFun"

atomToFun :: Lex -> [Definition] -> ([ExprTree SExpr] -> ExprTree SExpr)
atomToFun (Lex n m (LexAtom str)) def
    | str == "QUOTE" = quoteLsp
    | str == "EVAL" = (\args -> evalLsp args def) . (\ uncomputeLst -> computeArgs uncomputeLst def) --evalLsp . (\ uncomputeLst -> computeArgs uncomputeLst def) . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "CAR" = carLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "CDR" = cdrLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "CONS" = consLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "COND" = (\args -> condLsp args def)
    | str == "EQ" = eqLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "EQL" = eqlLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "ATOM" = atomLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "AND" = (\args -> andLsp args def)
    | str == "OR" = (\args -> orLsp args def)
    | str == "NOT" = notLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "PRINT" = printLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "NULL" = nullLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "CAAR" = caarLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "CADR" = cadrLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "CDDR" = cddrLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "CDDDR" = cdddrLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "CADAR" = cadarLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "APPEND" = appendLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "MEMBER" = memberLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | str == "INTERN" = internLsp . (\ uncomputeLst -> computeArgs uncomputeLst def)
    | otherwise = (defunLsp (Lex n m (LexAtom str)) def) . (\ uncomputeLst -> computeArgs uncomputeLst def)
atomToFun x _ = error ("Error: " ++ (show x) ++ " is not a function name")

computeExpr :: ExprTree SExpr -> [Definition] -> ExprTree SExpr
computeExpr (Leaf sexpr) _ = (Leaf sexpr)
computeExpr (Node []) _ = error "computeExpr"
computeExpr (Node (x:xl)) def = checkNil ((toFun x def) xl)

checkNil :: ExprTree SExpr -> ExprTree SExpr
checkNil (Leaf (SList [(SNil lex)])) = (Leaf (SNil lex))
checkNil x = x

computeTree :: ExprTree SExpr -> [Definition] -> ExprTree SExpr
computeTree (Leaf sexpr) def = computeExpr (Leaf sexpr) def
computeTree (Node (x:xl)) def = computeExpr (Node ((computeTree x def) : xl)) def

run :: [Definition] -> [Definition] -> [Definition]
run [] _ = []
run ((Fun fun):xl) def = (Fun fun) : run xl def
run ((Expr expr):xl) def= (Expr (computeTree expr def)) : (run xl def)

computeArgs :: [ExprTree SExpr] -> [Definition] -> [ExprTree SExpr]
computeArgs lst def = map (\tree -> computeTree tree def) lst

defunLsp :: Lex -> [Definition] -> ([ExprTree SExpr] -> ExprTree SExpr)
defunLsp lex def = defunLspHelp lex def def 

defunLspHelp :: Lex -> [Definition] -> [Definition] -> ([ExprTree SExpr] -> ExprTree SExpr)
defunLspHelp (Lex n m (LexAtom str)) [] def = error ("Not find function '" ++ str ++ "' at " ++ show n ++ "::" ++ show m)
defunLspHelp (Lex n m (LexAtom str)) ((Fun (UserFun name pnames body)):xl) def
    | str == name = (\ treeLst -> aply (UserFun name pnames body) treeLst def)
    | otherwise = defunLspHelp (Lex n m (LexAtom str)) xl def
defunLspHelp lex (x:xl) def = defunLspHelp lex xl def

aply :: UserFun -> [ExprTree SExpr] -> [Definition] -> ExprTree SExpr
aply (UserFun name pnames body) params def
    | length pnames /= length params = error ("Wrong number of arguments: '" ++ name ++ "'" ++ 
        " is expected to contain " ++ show (length pnames) ++ " argument.")
    | otherwise = deepSeqTree (map (\treeExpr -> computeTree (replaceLeafLst treeExpr pnames params) def) body)

quoteLsp :: [ExprTree SExpr] -> ExprTree SExpr
quoteLsp [x] = flattenTree x
quoteLsp _ = error ("Wrong number of arguments: 'quote'" ++ 
        " is expected to contain 1 argument.")

evalLsp :: [ExprTree SExpr] -> [Definition] -> ExprTree SExpr
evalLsp [(Leaf (SList lst))] def = computeTree (Node (makeLeafList lst)) def
evalLsp arg _ = error ("Wrong number of arguments: 'eval'" ++ 
        " is expected to contain 1 argument." ++ show arg)

makeLeafList :: [SExpr] -> [ExprTree SExpr]
makeLeafList [] = []
makeLeafList [(SNil expr)] = []
makeLeafList ((SList lst):xl) = (Node ((makeLeafList lst) ++ (makeLeafList xl))) : []
makeLeafList (x:xl) = (Leaf x) : makeLeafList xl

carLsp :: [ExprTree SExpr] -> ExprTree SExpr
carLsp [(Leaf (SList (x:xl)))] = (Leaf x)
carLsp [(Leaf sexpr)] = error ("Wrong type argument: It is expected that the 'car'" ++ (show sexpr) ++
        "  argument has the type 'list'.")
carLsp lst
    | allLeaf lst = error ("Wrong number of arguments: 'car'" ++ 
        " is expected to contain 1 argument.")
    | otherwise = error "Error: carLsp"

cdrLsp :: [ExprTree SExpr] -> ExprTree SExpr
cdrLsp [(Leaf (SList [x]))] = (Leaf x)
cdrLsp [(Leaf (SList (x:xl)))] = (Leaf (SList xl))
cdrLsp [(Leaf _)] = error ("Wrong type argument: It is expected that the 'cdr'" ++ 
        "  argument has the type 'list'.")
cdrLsp lst
    | allLeaf lst = error ("Wrong number of arguments: 'cdr'" ++ 
        " is expected to contain 1 argument.")
    | otherwise = error "Error: cdrLsp"

consLsp :: [ExprTree SExpr] -> ExprTree SExpr
consLsp ((Leaf sexpr):[(Leaf (SList lst))]) = (Leaf (SList (sexpr : lst)))
consLsp ((Leaf sexpr):[(Leaf (SNil lex))]) = (Leaf (SList (sexpr : [(SNil lex)])))
consLsp ((Leaf sexpr):[(Leaf _)])  = error ("Wrong type argument: It is expected that the 'cond'" ++ 
        "  second argument has the type 'list'.")
consLsp lst
    | allLeaf lst = error ("Wrong number of arguments: 'cond'" ++ 
        " is expected to contain 2 argument.")
    | otherwise = error "Error: consLsp"

condLsp :: [ExprTree SExpr] -> [Definition] -> ExprTree SExpr
condLsp [] def = (Leaf (SNil (Lex (-1) (-1) LexNil)))
condLsp ((Node (pred:[expr])):xl) def
    | isEqExprTree (Leaf (SAtom (Lex (-1) (-1) LexT))) $ computeTree pred def = computeTree expr def
    | otherwise = condLsp xl def
condLsp _ _ = error "Error: uncorrect count of args for 'cond'"

eqLsp :: [ExprTree SExpr] -> ExprTree SExpr
eqLsp (exprTree1:[exprTree2])
    | isEqExprTree exprTree1 exprTree2 = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | otherwise = (Leaf (SNil (Lex (-1) (-1) LexNil)))
eqLsp _ = error "Error: uncorrect count of args for 'eq'"

eqlLsp :: [ExprTree SExpr] -> ExprTree SExpr
eqlLsp lst = eqLsp lst

atomLsp :: [ExprTree SExpr] -> ExprTree SExpr
atomLsp [(Leaf (SList _))] = (Leaf (SNil (Lex (-1) (-1) LexNil)))
atomLsp [x] = (Leaf (SAtom (Lex (-1) (-1) LexT)))
atomLsp _ = error "Error: uncorrect count of args for 'atom'"

andLsp :: [ExprTree SExpr] -> [Definition] -> ExprTree SExpr
andLsp [] def = (Leaf (SAtom (Lex (-1) (-1) LexT)))
andLsp (x:xl) def
    | isEqExprTree (computeTree x def) (Leaf (SNil (Lex (-1) (-1) LexNil))) = (Leaf (SNil (Lex (-1) (-1) LexNil)))
    | otherwise = andLsp xl def

orLsp :: [ExprTree SExpr] -> [Definition] -> ExprTree SExpr
orLsp [] def = (Leaf (SNil (Lex (-1) (-1) LexNil)))
orLsp (x:xl) def
    | isEqExprTree (computeTree x def) (Leaf (SNil (Lex (-1) (-1) LexNil))) = orLsp xl def
    | otherwise = (Leaf (SAtom (Lex (-1) (-1) LexT)))

notLsp :: [ExprTree SExpr] -> ExprTree SExpr
notLsp [(Leaf (SNil _))] = (Leaf (SAtom (Lex (-1) (-1) LexT)))
notLsp [x] = (Leaf (SNil (Lex (-1) (-1) LexNil)))
notLsp _ = error "Error: uncorrect count of args for 'not'"

equsLsp :: [ExprTree SExpr] -> ExprTree SExpr
equsLsp (x:[y])
    | isInt x && isInt y && getInt x == getInt y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isFloat x && isInt y && getFloat x == fromIntegral (getInt y) = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isInt x && isFloat y && fromIntegral (getInt x) == getFloat y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isFloat x && isFloat y && getFloat x == getFloat y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | otherwise = (Leaf (SNil (Lex (-1) (-1) LexNil)))
equsLsp _ = error "Error: uncorrect count of args for '='"

addLsp :: [ExprTree SExpr] -> ExprTree SExpr
addLsp (x:[y])
    | isInt x && isInt y = (Leaf (SAtom (Lex (-1) (-1) (LexIntAtom (getInt x + getInt y)))))
    | isFloat x && isInt y = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x + fromIntegral (getInt y))))))
    | isInt x && isFloat y = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral (getInt x) + getFloat y)))))
    | otherwise = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x + getFloat y)))))
addLsp _ = error "Error: uncorrect count of args for '+'"

subLsp :: [ExprTree SExpr] -> ExprTree SExpr
subLsp [x] = subLspHelpI [x] (Leaf (SAtom (Lex (-1) (-1) (LexIntAtom (0)))))
subLsp (x:xl) = subLspHelpI xl x
subLsp _ = error "Error: uncorrect count of args for '-'"

subLspHelpI :: [ExprTree SExpr] -> ExprTree SExpr -> ExprTree SExpr
subLspHelpI [y] x
    | isInt x && isInt y = (Leaf (SAtom (Lex (-1) (-1) (LexIntAtom (getInt x - getInt y)))))
    | isFloat x && isInt y = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x - fromIntegral (getInt y))))))
    | isInt x && isFloat y = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral (getInt x) - getFloat y)))))
    | otherwise = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x - getFloat y)))))
subLspHelpI (y:yl) x
    | isInt x && isInt y = subLspHelpI yl (Leaf (SAtom (Lex (-1) (-1) (LexIntAtom (getInt x - getInt y)))))
    | isFloat x && isInt y = subLspHelpF yl (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x - fromIntegral (getInt y))))))
    | isInt x && isFloat y = subLspHelpF yl (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral (getInt x) - getFloat y)))))
    | otherwise = subLspHelpF yl (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x - getFloat y)))))
subLspHelpI _ _= error "Error: uncorrect count of args for '-'"

subLspHelpF :: [ExprTree SExpr] -> ExprTree SExpr -> ExprTree SExpr
subLspHelpF [y] x
    | isFloat x && isInt y = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x - fromIntegral (getInt y))))))
    | otherwise = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x - getFloat y)))))
subLspHelpF (y:yl) x
    | isFloat x && isInt y = subLspHelpF yl (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x - fromIntegral (getInt y))))))
    | otherwise = subLspHelpF yl (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x - getFloat y)))))
subLspHelpF _ _= error "Error: uncorrect count of args for '-'"

mulLsp :: [ExprTree SExpr] -> ExprTree SExpr
mulLsp (x:[y])
    | isInt x && isInt y = (Leaf (SAtom (Lex (-1) (-1) (LexIntAtom (getInt x * getInt y)))))
    | isFloat x && isInt y = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x * fromIntegral (getInt y))))))
    | isInt x && isFloat y = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral (getInt x) * getFloat y)))))
    | otherwise = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x * getFloat y)))))
mulLsp _ = error "Error: uncorrect count of args for '*'"

divLsp :: [ExprTree SExpr] -> ExprTree SExpr
divLsp (x:[y])
    | isInt x && isInt y = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (1.0 * fromIntegral (getInt x) / fromIntegral (getInt y))))))
    | isFloat x && isInt y = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x / fromIntegral (getInt y))))))
    | isInt x && isFloat y = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (fromIntegral (getInt x) / getFloat y)))))
    | otherwise = (Leaf (SAtom (Lex (-1) (-1) (LexFloatAtom (getFloat x / getFloat y)))))
divLsp _ = error "Error: uncorrect count of args for '/'"

grtLsp :: [ExprTree SExpr] -> ExprTree SExpr
grtLsp (x:[y])
    | isInt x && isInt y && getInt x > getInt y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isFloat x && isInt y && getFloat x > fromIntegral (getInt y) = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isInt x && isFloat y && fromIntegral (getInt x) > getFloat y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isFloat x && isFloat y && getFloat x > getFloat y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | otherwise = (Leaf (SNil (Lex (-1) (-1) LexNil)))
grtLsp _ = error "Error: uncorrect count of args for '>'"

lssLsp :: [ExprTree SExpr] -> ExprTree SExpr
lssLsp (x:[y])
    | isInt x && isInt y && getInt x < getInt y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isFloat x && isInt y && getFloat x < fromIntegral (getInt y) = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isInt x && isFloat y && fromIntegral (getInt x) < getFloat y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isFloat x && isFloat y && getFloat x < getFloat y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | otherwise = (Leaf (SNil (Lex (-1) (-1) LexNil)))
lssLsp _ = error "Error: uncorrect count of args for '<'"

goeLsp :: [ExprTree SExpr] -> ExprTree SExpr
goeLsp (x:[y])
    | isInt x && isInt y && getInt x >= getInt y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isFloat x && isInt y && getFloat x >= fromIntegral (getInt y) = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isInt x && isFloat y && fromIntegral (getInt x) >= getFloat y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isFloat x && isFloat y && getFloat x >= getFloat y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | otherwise = (Leaf (SNil (Lex (-1) (-1) LexNil)))
goeLsp _ = error "Error: uncorrect count of args for '>='"

loeLsp :: [ExprTree SExpr] -> ExprTree SExpr
loeLsp (x:[y])
    | isInt x && isInt y && getInt x <= getInt y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isFloat x && isInt y && getFloat x <= fromIntegral (getInt y) = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isInt x && isFloat y && fromIntegral (getInt x) <= getFloat y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | isFloat x && isFloat y && getFloat x <= getFloat y = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | otherwise = (Leaf (SNil (Lex (-1) (-1) LexNil)))
loeLsp _ = error "Error: uncorrect count of args for '<='"

printLsp :: [ExprTree SExpr] -> ExprTree SExpr
printLsp [(Leaf sexpr)] = printLeaf (Leaf sexpr)
printLsp lst
    | allLeaf lst = error ("Wrong number of arguments: 'print'" ++ 
        " is expected to contain 1 argument.")
    | otherwise = error ("Error: printLsp" ++ (show lst))

--

nullLsp :: [ExprTree SExpr] -> ExprTree SExpr
nullLsp [x] = eqlLsp (x:[(Leaf (SNil (Lex (-1) (-1) LexNil)))])
nullLsp _ = error "Error: uncorrect count of args for 'null'"

caarLsp :: [ExprTree SExpr] -> ExprTree SExpr
caarLsp lst = carLsp $ (carLsp lst : [])

cadrLsp :: [ExprTree SExpr] -> ExprTree SExpr
cadrLsp lst = carLsp $ (cdrLsp lst : [])

cddrLsp :: [ExprTree SExpr] -> ExprTree SExpr
cddrLsp lst = cdrLsp $ (cdrLsp lst : [])

cdddrLsp :: [ExprTree SExpr] -> ExprTree SExpr
cdddrLsp lst = cdrLsp (cdrLsp(cdrLsp lst:[]):[])

cadarLsp :: [ExprTree SExpr] -> ExprTree SExpr
cadarLsp lst = carLsp $ (cdrLsp (carLsp lst : []) : [])

appendLsp :: [ExprTree SExpr] -> ExprTree SExpr
appendLsp ((Leaf (SNil _)):[(Leaf (SList lst2))]) = (Leaf (SList lst2))
appendLsp ((Leaf (SList lst1)):[(Leaf (SList lst2))]) = (Leaf (SList ((takeL lst1 0 (-2)) ++ lst2)))
appendLsp ((Leaf (SList lst1)):[(Leaf (SNil _))]) = (Leaf (SList lst1))
appendLsp (x:xs:xl)  = error ("Wrong number of arguments: 'append'" ++
        " is expected to contain 2 argument.")
appendLsp _  = error ("Wrong type of arguments: It is expected that the 'append'" ++ 
        "  arguments have the type 'list'.")

memberLsp :: [ExprTree SExpr] -> ExprTree SExpr
memberLsp ((Leaf expr):[(Leaf (SNil _))]) = (Leaf (SNil (Lex (-1) (-1) LexNil)))
memberLsp ((Leaf expr):[(Leaf (SList (x:xl)))])
    | isEqExpr x expr = (Leaf (SAtom (Lex (-1) (-1) LexT)))
    | otherwise = memberLsp ((Leaf expr) : (checkNil (Leaf (SList (xl)))) : [])
memberLsp (x:xs:xl)  = error ("Wrong number of arguments: 'member'" ++
        " is expected to contain 2 argument.")
memberLsp _  = error ("Wrong type of arguments: It is expected that the 'member'" ++ 
        "  arguments have the type 'list'.")

internLsp :: [ExprTree SExpr] -> ExprTree SExpr
internLsp ((Leaf (SAtom (Lex _ _ (LexAtom str1)))):[(Leaf (SAtom (Lex _ _ (LexAtom str2))))]) = (Leaf (SAtom (Lex (-1) (-1) (LexAtom (str1 ++ str2)))))
internLsp ((Leaf (SAtom (Lex _ _ (LexAtom str1)))):[(Leaf (SAtom (Lex _ _ (LexIntAtom num))))]) = (Leaf (SAtom (Lex (-1) (-1) (LexAtom (str1 ++ show num)))))
internLsp (x:xs:xl)  = error ("Wrong number of arguments: 'intern'" ++
        " is expected to contain 2 argument." ++ show xs ++ show xl)
internLsp _  = error ("Wrong type of arguments: It is expected that the 'intern'" ++ 
        " arguments have the type 'list'.")




-- [Leaf (SList [SAtom (Lex {posSTR = 0, posSYM = 19, lexType = LexQuote}),
	-- SList [SAtom (Lex {posSTR = 0, posSYM = 21, lexType = LexAtom "CDR"}),
		-- SList [SAtom (Lex {posSTR = 0, posSYM = 26, lexType = LexAtom "A"}),
			-- SAtom (Lex {posSTR = 0, posSYM = 28, lexType = LexAtom "B"}),
			-- SAtom (Lex {posSTR = 0, posSYM = 30, lexType = LexAtom "C"}),
		-- SNil (Lex {posSTR = -1, posSYM = -1, lexType = LexNil})]
	-- ,SNil (Lex {posSTR = -1, posSYM = -1, lexType = LexNil})]
-- ,SNil (Lex {posSTR = -1, posSYM = -1, lexType = LexNil})])

-- ,Leaf (SNil (Lex {posSTR = -1, posSYM = -1, lexType = LexNil}))]