module Main where

import Lib
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Control.Monad.Except


data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String
instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either LispError --remember, this iss a "type synonym" (kinda an alias for a type)
--note that this type is 'curried' - a full type would be e.g. Either LispError Integer, Either LispError LispVal etc
--so ThrowsError can be applied to any data type now.

trapError action = catchError action (return . show)
--catchError :: MonadError e m => m a -> (e -> m a) -> m a
--takes an Either action and a function that turns an error into another Either action.
--(here, that function is (return.show), which gets the string repr and then lifts that into the Either monad, i think)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val --we assume here that the Either is always a Right.  i think that's bc "show" always gets a string?
-- this is bc we only intend to use extractValue after a trapError (which results in (return . show))
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

--define a parser action that accepts a backslash followed by an escaped char
escapedChars :: Parser Char
escapedChars = do
    char '\\' -- signifies the beginning of an escaped char (in haskell, we also can write a literal backslash char but have to escape it in the literal)
    x <- oneOf "\\\"nrt" -- \\ is escaped backslash, \" is escaped doublequote
    -- oneOf gets a single instance of either \ or "
    return $ case x of
        '\\' -> x
        '"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String x --String x constructs a LispVal, applying return to this creates a Parser LispVal

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest --use cons instead of ++ since first is just a char
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom --underscore here matches anything (wildcard)

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float (fst . head $ readFloat ( x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio ((read x) % (read y ))

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin
--parseNumber = liftM (Number . read) $ many1 digit  --parse many digits.  apply "read" to this
--pass the result to Number constructor.  
--we want this all to be inside of the Parser monad, (many1 digit produces a Parser String, not a String)
--therefore liftM lifts this function into monad form
--here is my attempt to rewrite parseNumber in do notation?

parseDecimal1 :: Parser LispVal
parseDecimal1 = do
    --(return . Number . read) num
    num <- many1 digit
    let i0 = read num
    let i1 = Number i0
    return i1

parseDecimal2 :: Parser LispVal
parseDecimal2 = do  -- version of decimal prefixed by #d
    try $ string "#d"
    x <- many1 digit
    (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (hex2dig x)

hex2dig x = fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number (oct2dig x)

oct2dig x = fst $ readOct x !! 0

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- many1 (oneOf "10")
    return $ Number (bin2dig x)

bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
    bin2dig' old xs

parseExpr :: Parser LispVal
parseExpr = parseAtom --accept any of the following parsed types
        <|> parseString
        <|> try parseRatio --the "try" combinator allows backtracking.
        --that is, it attempts to run the specified parser, but if that fails,
        -- it backs up to the previous state.
        -- therefore we can use it in a choice alternative without interfering with any other alternatives
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> do
            char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x

        --the "try" is needed because parseNumber, parseBool, and parseCharacter can all start with hash

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space")
            <|> do 
                x <- anyChar
                notFollowedBy alphaNum
                return [x]
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> (value !! 0)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces
--this is the List data constructor for LispVal
--we liftM this into a Parser LispVal
--sepBy p sep parses zero or more occurrences of p, separated by sep. Returns a list of values returned by p.

parseDottedList :: Parser LispVal
-- of the form (a b c d . f)
-- so get 'a b c d ' as the "head"
-- then get 'f' as the "tail"
parseDottedList = do
    head <- endBy parseExpr spaces --endBy p sep parses zero or more occurrences of p, separated and ended by sep. Returns a list of values returned by p.
    tail <- char '.' >> spaces >> parseExpr --i think here we ignore the result of char '.' and spaces, and just bind the result of parseExpr to tail
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\'' --syntactic sugar for (quote x)  is  'x
    x <- parseExpr
    return $ List [Atom "quote", x]

showVal :: LispVal -> String --using pattern matching on LispVal data constructors to define showVal to convert LispVal to String
showVal (String contents) = "\"" ++ contents ++ "\"" --for the String data constructor
showVal (Atom name) = name --for the Atom data constructor
showVal (Number contents) = show contents --show :: Show a => a -> String
showVal (Bool True) = "#t" --here "Bool" is a LispVal data constructor, where True is a Haskell value True, and this tells showVal to match that input to the literal "#t"
showVal (Bool False) = "#f" --note that this Bool cases are matching beyond simply the data constructor!  it matches the Bool data constructor and then further matches the value (True or False)!

showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal --declaring/defining LispVal to be an instance of Show typeclass

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err --here Parser is a data constructor of LispError
    Right val -> return val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal -- apply showVal to every LispVal in the list, then apply unwords to that list of strings

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Float Double
            | Ratio Rational
            | Number Integer
            | String String
            | Bool Bool
            | Character Char

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val --this val@(String _) pattern matches any LispVal with the String constructor, binds "val" as a LispVal instead of just a bare String.
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val --the eval of (quote val) AKA 'val is val
eval (List (Atom func : args)) = mapM eval args >>= apply func --evaluate all arguments (expressions past the first expression, which is function) then apply function to result
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args) 
    (lookup func primitives)
--($ x) = (\y -> y $ x) = flip ($) x
--so ($ args) implicitly creates a lambda that applies its argument (a function) to args
--lookup returns a Maybe function, i believe.
--"maybe :: b -> (a -> b) -> Maybe a -> b"
-- here, "b" default value is (Bool False)
-- function (a->b) is ($ args), the lambda that applies its argument to args
-- the Maybe value "Maybe a" is the result of "lookup func primitives"
-- if that Maybe result is not Nothing, then maybe applies the function (previously mentioned lambda) to the value inside the Just

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem)]
--numericBinop takes a primitive Haskell function and wraps it with the ability to unpack an argument list, apply the function to the values from that, and return a result of the Number constructor type
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
    if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
        --the above is actually confusing.  reads is "equivalent to readsPrec with a precedence of 0."
        --reads n is String -> [(a, String)]
        --so applying  reads n to a String would give [(a,String)]
        --and that is why we need to declare the type of 
        --reads n :: [(Integer, String)]
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr ( args !! 0 ) >>= eval
    putStrLn $ extractValue $ trapError evaled
