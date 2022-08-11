module Main where

import Lib
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio


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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

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

eval :: LispVal -> LispVal
eval val@(String _) = val --this val@(String _) pattern matches any LispVal with the String constructor, binds "val" as a LispVal instead of just a bare String.
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val --the eval of (quote val) AKA 'val is val
eval (List (Atom func : args)) = apply func $ map eval args--evaluate all arguments (expressions past the first expression, which is function) then apply function to result

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives
--($ x) = (\y -> y $ x) = flip ($) x
--so ($ args) implicitly creates a lambda that applies its argument (a function) to args
--lookup returns a Maybe function, i believe.
--"maybe :: b -> (a -> b) -> Maybe a -> b"
-- here, "b" default value is (Bool False)
-- function (a->b) is ($ args), the lambda that applies its argument to args
-- the Maybe value "Maybe a" is the result of "lookup func primitives"
-- if that Maybe result is not Nothing, then maybe applies the function (previously mentioned lambda) to the value inside the Just

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem)]
--numericBinop takes a primitive Haskell function and wraps it with the ability to unpack an argument list, apply the function to the values from that, and return a result of the Number constructor type
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
    if null parsed
        then 0
        else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
