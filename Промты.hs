Все работает! Теперь приведи к "красивому виду" данные файлы:
Длинные строчки кода (больше 80 символов) подразбей на более короткие строки.
 Для этого используй ТОЛЬКО where.
 Самое главное - не меняй ничего другого, логика не должна поменяться.
 

Почему тут при интерактивной интерпретации не возникает ошибка о множественном определении?
>>>(defun g (a) a)
>>>(defun g (a) "aaaaaaaa")
>>>(print (g "afffff"))
"aaaaaaaa"
А правильное ли это поведение?

Укажи основные отличия в организации вычислений функциональных выражений в лисп и хаскель

Является ли грамматика допустимой для рекурсивного спуска:
S -> (L)
L-> aL | S | Eps

Напиши грамматику (для которой применим метод рекурсивного спуска), которая определяет вложенные списочные выражения:
1) пустой список: ()
2) элементы списка перечисляются через пробел: (el1 el2 el3)
3) элементы могут быть списками

Haskel
Я хочу сделать тип данных Lex, который является перечислимым типом, но при некоторых конструкторах есть параметры:
data Lex = LexNil | LexT | LexAtom | LexAtomInt Int | Lex AtomFloat Float
Как это правильно реализовать?

У меня есть следующая функция. При совпадении аргумента с прототипом (x:xs:xl) я должен проверить, являются ли первые 2 символа строки двойным знаком сравнения, если нет, то нужно продолжить парсить строчку (дальше уже двойных знаков нет, используется только перввый символ - достаточно протита (x:xl). 
Проблема: мне приходится дублировать протип (x:xl) в (x:xs:xl). Как избавиться от этого дуюлирования?
parse :: String -> [Lex]
parse [] = LexEof : []
parse (x:xs:xl)| x == '>' && xs == '=' = LexGoE : parse xl
            | x == '>' = LexGrt : parse (xs:xl)
            | x == '<' && xs == '=' = LexLoE : parse xl
            | x == '<' = LexLss : parse (xs:xl)
            
            | isSpace x = parse xl
            | x == ';' = parseComment (x:xs:xl)
            | isAlpha x || x == '_' = parseAtom (x:xs:xl) []
            | isDigit x = parseDigit (x:xs:xl) []
            | x == '\"' = parseDigit (x:xs:xl) []
            | x == '(' = LexOP : parse (xs:xl)
            | x == ')' = LexCP : parse (xs:xl)
            | x == '`' = LexQuote : parse (xs:xl)
            | x == '.' = LexPoint : parse (xs:xl)
            | x == '+' = LexPlus : parse (xs:xl)
            | x == '-' = LexMin : parse (xs:xl)
            | x == '/' = LexDiv : parse (xs:xl)
            | x == '*' = LexMul : parse (xs:xl)
            | x == '=' = LexEq : parse (xs:xl)
            | error      
parse (x:xl)| isSpace x = parse xl
            | x == ';' = parseComment (x:xl)
            | isAlpha x || x == '_' = parseAtom (x:xl) []
            | isDigit x = parseDigit (x:xl) []
            | x == '\"' = parseDigit (x:xl) []
            | x == '(' = LexOP : parse xl
            | x == ')' = LexCP : parse xl
            | x == '`' = LexQuote : parse xl
            | x == '.' = LexPoint : parse xl
            | x == '+' = LexPlus : parse xl
            | x == '-' = LexMin : parse xl
            | x == '/' = LexDiv : parse xl
            | x == '*' = LexMul : parse xl
            | x == '=' = LexEq : parse xl
            | error
			


Как исправить: parse error (possibly incorrect indentation or mismatched brackets)
   |
65 | parseComment :: String -> [Lex]
   | ^
Код:
parseComment :: String -> [Lex]
parseComment [] = LexEof : []
parseComment (x:xl) | x != '\n' = parseComment xl
                    | otherwise = parse xl
					
					

Почему возникает ошибка в коде:
main :: IO ()
main = do
    putStrLn "Enter program:"
    pgm <- getLine
    print(map show $ traceTokens $ parse pgm)
data LexType =    LexOP                 {- ( -}
            | LexCP                 {- ) -}
            | LexNil
            | LexT
            | LexAtom String
            | LexStringAtom String
            | LexIntAtom Int
            | LexFloatAtom Float
            | LexQuote              {- ` -}
            | LexPoint              {- . -}
            | LexPlus               {- + -}
            | LexMin                {- - -}
            | LexDiv                {- / -}
            | LexMul                {- * -}
            | LexEq                 {- = -}
            | LexGrt                {- > -}
            | LexGoE                {- >= -}
            | LexLss                {- < -}
            | LexLoE                {- <= -}
            | LexEof
            deriving (Show)

data Lex = Lex {posSTR::Int, posSYM::Int, lexType::LexType}
traceTokens :: [Lex] -> [Lex]
traceTokens tokens = 
    trace (intercalate "\n" (map show tokens)) tokens
parse :: String -> Int -> Int -> [Lex]
parse [] posSTR posSYM = [(Lex posSTR posSYM LexEof)]
parse (x:xs:xl) posSTR posSYM
    | x == '>' && xs == '=' = (Lex posSTR posSYM LexGoE) : parse xl posSTR (posSYM + 2)
    | x == '>' = (Lex posSTR posSYM LexGrt) : parse (xs:xl) posSTR (posSYM + 1)
    | x == '<' && xs == '=' = (Lex posSTR posSYM LexLoE) : parse xl posSTR (posSYM + 2)
    | x == '<' = (Lex posSTR posSYM LexLss) : parse (xs:xl) posSTR (posSYM + 1)
    | otherwise = parseHelp (x:xs:xl) posSTR posSYM        
parse str = parseHelp str posSTR posSYM
jdoodle.hs:42:1: error:
    Equations for ‘parse’ have different numbers of arguments
      jdoodle.hs:42:1-53
      jdoodle.hs:49:1-39
   |
42 | parse [] posSTR posSYM = [(Lex posSTR posSYM LexEof)]
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...




Это правильная реализация n-арного дерева?
data Tree a = Node [(Tree a)]| Nothing







Можно ли написать подключение к классу короче:
data LexType = LexOP                 {- ( -}
            | LexCP                 {- ) -}
            | LexNil
            | LexT
            | LexAtom String
            | LexStringAtom String
            | LexIntAtom Int
            | LexFloatAtom Float
            | LexQuote              {- ` -}
            | LexPoint              {- . -}
            | LexAdd                {- + -}
            | LexSub                {- - -}
            | LexDiv                {- / -}
            | LexMul                {- * -}
            | LexEq                 {- = -}
            | LexGrt                {- > -}
            | LexGoE                {- >= -}
            | LexLss                {- < -}
            | LexLoE                {- <= -}
            | LexEof
            deriving (Show)

instance Eq LexType where
    LexOP == LexOP = True
	LexCP == LexCP = True
	...
	_ == _ = False


В чем ошибка?
jdoodle.hs:214:20: error: parse error on input ‘->’
    |
214 | takeListHelp [Lex] -> [Lex] -> Int -> [Lex]
    |    

takeList :: [Lex] -> [Lex]
takeList [] = []
takeList (x:xl)
    | eqLexTypes x LexOP = takeListHelp xl [] 1 
    | otherwise = [x]

takeListHelp [Lex] -> [Lex] -> Int -> [Lex]
takeListHelp [] [] _ = []
takeListHelp [] lst _ = lst
takeListHelp (x:xl) lst n 
    | n == 0 =  lst
    | eqLexTypes x LexOP = takeListHelp xl (lst ++ x) (n + 1)
	| eqLexTypes x LexCP = takeListHelp xl (lst ++ x) (n - 1)
	| otherwise = takeListHelp xl (lst ++ x) n
	
В следующем коде правильно описана функция computeTree? Особенно интересует подстановка функции computeTree (Node (x:xl)).


Как мне реализовать такую структуру: Обертка над типом Lex - тип Atom просто содержит Lex. AtomList содержит непустой список Atom или же AtomNil. Elemnt может быть Atom  или AtomList. Примерная реализация:
data Element = Atom | AtomList
data Atom = Lex
data AtomList = Atom [Atom Lex] | AtomNil