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
        <|> try parseRatio
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter

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

readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Float Double
            | Ratio Rational
            | Number Integer
            | String String
            | Bool Bool
            | Character Char

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
