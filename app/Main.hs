{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Lib
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Control.Monad.Except --we are basically using a super-set of the Either monad to handle errors.
--note: "It is common to use Either String as the monad type constructor for an error monad in which error descriptions take the form of strings. In that case and many other common cases the resulting monad is already defined as an instance of the MonadError class.
-- i think here, it's quite similar, tho we're using Either LispVal, and our LispVal is an instance of Show, so pretty similar
import System.IO
import Data.IORef --for storing state, like the arbitrarily nested environment

type Env = IORef [(String, IORef LispVal)] --mappings from string to (mutable) values
-- in scheme, (set! changes the value of a variable
-- and (define adds a new string,value pair to env

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO --ExceptT is a monad transformer.  we combine multiple monads.
--we layer error handling (LispError) on top of the IO monad.
--Like ThrowsError, IOThrowsError is really a type constructor: we've left off the last argument, the return type of the function
--a monad that may contain IO actions that throw a LispError
--We have a mix of ThrowsError and IOThrowsError functions, but actions of different types cannot be contained within the same do-block, even if they provide essentially the same functionality

liftThrows :: ThrowsError a -> IOThrowsError a --destructures the Either type and either re-throws the error type (superset of Either, basically) or returns the ordinary value
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
--take any error values and convert them to their string representations
--runExceptT :: ExceptT e m a -> m (Either e a)Source# The inverse of ExceptT.
--newtype ExceptT e m a
--A monad transformer that adds exceptions to other monads.
--ExceptT constructs a monad parameterized over two things:
--e - The exception type.
--m - The inner monad.
--so, runExceptT is probably a way to get at the "inner" monad
---- | Map the unwrapped computation using the given function.


isBound :: Env -> String -> IO Bool --is a given variable already bound in the given environment?
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do 
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "uh-oh, unbound variable" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
--note: setVar is a big difference between Scheme (which has set! and mutable state) and Haskell (which ... does not... have mutable state)
--we use IORef to provide "mutable references in the IO monad" to work around this need for representing mutable state
setVar envRef var value = do 
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "uh-oh, an unbound variable" var) (liftIO . (flip writeIORef value)) (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value --var already bound, so update its value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env) -- i think this is where the rubber meets the rode, we're adding the new var, valueRef pair here
            return value

--list version of defineVar, i think.  defines bunch of vars from the list at once
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where 
        extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do 
            ref <- newIORef value
            return (var, ref)

flushStr :: String -> IO () --print out a string.  flush the buffer if needed
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env --liftM :: Monad m => (a1 -> r) -> m a1 -> m r.  so we apply liftM to (show) which is of type (Show a => a -> String).  the monad is related to error, i think.
--throwsError LispVal is the type of eval
--show : showError :: LispError -> String
-- liftM must lift show into throwsError LispError -> throwsError String i guess
--type ThrowsError = Either LispError
--trapError :: (MonadError e m, Show e) => m String -> m String
--extractValue :: extractValue :: ThrowsError a -> a (here, i guess it's a String)
--return:: lift it into IO string

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
    result <- prompt
    if pred result --predicate that signals when to stop
        then return () 
        --if not stop, then apply action to the prompt thing, then loop
        else action result >> until_ pred prompt action --if not stop, loop (here looping by recursing)

--updating runOne to take name of a file to execute & run that as a program
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
        >>= hPutStrLn stderr
--runOne :: String -> IO ()
--runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "scheme2hask:>>> ") . evalAndPrint

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

zspaces :: Parser ()
zspaces = skipMany space

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
        <|> try zparseList --upgrading parseList and parseDottedList because they don't allow leading or trailing whitespace after ( or before ) !
        <|> try zparseDottedList
        <|> try zparseComment

zparseComment :: Parser LispVal
zparseComment = do
    s <- semiComment
    return $ Comment s --really, I'm not even sure this should be a LispVal.  But we're going to try this out.  I'd prefer to handle comments in some pre-processing step.  yet here we are...

semiComment = do
    manyTill space (lookAhead (char ';' ))
    char ';'
    manyTill anyChar (lookAhead newline)

zparseList = do
  char '('
  zspaces
  x <- try parseList <|> parseListWS --tricky case ending list with sdf ) vs sdf)
  zspaces
  char ')'
  --zspaces
  optional (try semiComment)
  return x

zparseDottedList = do
  char '('
  zspaces
  x <- parseDottedList
  zspaces
  char ')'
  --zspaces
  --optional semiComment
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
parseList = do
  head <- sepBy parseExpr spaces --if terminated by sdf), we're done here.
  return $ List head
parseListWS :: Parser LispVal
parseListWS = do
  head <- endBy parseExpr spaces --if terminated by sdf ), we're done here.
  return $ List head
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
showVal (PrimitiveFunc _ ) = "<primitive>" --for brevity, only show the fact that the value is a primitive.
--note: in the following, the LHSs inside the {} are the record attributes of LispVal FunFunc, and the RHS are holding their values!  so it's like a backwards-assignment (the right-hand-side gets the value starting in the LHS) compared to something like C++
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
    "(lambda (" ++ unwords (map show args ) ++ (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
        --if there's a variadic argument, show it after a dot
showVal (Port _ ) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Comment s) = "<comment>"

instance Show LispVal where show = showVal --declaring/defining LispVal to be an instance of Show typeclass
{-
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err --here Parser is a data constructor of LispError
    Right val -> return val
-} --updated readExpr when we generalized to loading files

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
            | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
            | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env} 
            --using "record syntax" lets us store data kinda like key-value store or attributes.  more easily keeps track of the info that we wanna store.
            --note that we are storing the function body as a list of LispVal (list of expressions, since expressions are essentially LispVal)
            --vararg is, if the function is variadic, the name of the variable holding the list of parameters for the variadic part.
            | IOFunc ([LispVal] -> IOThrowsError LispVal) --for primitives that handle I/O
            | Port Handle --Ports represent input and output devices. To Scheme, an input port is a Scheme object that can deliver characters upon command, while an output port is a Scheme object that can accept characters.
            | Comment String

--helper functions for evaluating function definitions ((define ...) and (lambda ...))   
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

--have to update eval from ThrowsError (monad) to IOThrowsError (from monad transformer)
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val--this val@(String _) pattern matches any LispVal with the String constructor, binds "val" as a LispVal instead of just a bare String.
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env val@(Comment c) = return val --this is ugly but we're trying to figure out how to handle "comments" , which should really be "do nothing"
eval env (List [Atom "quote", val]) = return val --the eval of (quote val) AKA 'val is val
eval env (List [Atom "if", pred, conseq, alt]) =
    do 
        result <- eval env pred
        case result of--the last statement (this case statement) in a do block will be the overall result of the do block.  eval either alt or conseq based on value of eval pred
            Bool False -> eval env alt
            otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
--following are evaluations of function definition expressions
--the compiler actually gives warning: [-Woverlapping-patterns] Pattern match is redundant
--  i fixed this by moving the pattern for function application down after these.  maybe it gets caught in function application, essentially matching "define" or "lambda" as a function to apply.  that does sound like it could be potentially bad...
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) = 
    load filename >>= liftM last . mapM (eval env) --handle the Scheme load as a special procedure rather than a general function case.
    --we do this because the "load" "function" can introduce bindings into the environment, but our "apply" does not take an environment argument, so our notion of 'function' cannot as-is modify the environment in which it is called.

eval env (List (function : args)) = do --function application.  one eval covers everything!
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

applyProc :: [LispVal] -> IOThrowsError LispVal --wrapper around "apply" to destructure argument list into the form "apply" expects 
applyProc [func, List args] = apply func args -- we use applyProc when we see "apply" in Scheme source (see ioPrimitives list)
applyProc (func : args) = apply func args

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args --note that IOFunc has a different type than PrimitiveFUnc! [LispVal] -> ThrowsError LispVal instead of IOThrowsError LispVal.  that's why we need to lift the result of a PrimitiveFunc but not of a IOFunc
apply (Func params varargs body closure) args =
    if (num params /= num args) && (varargs == Nothing) --need number of params (formals) to be equal to the number of args we're applying it to.
        --note that this indicates that in Scheme we are not rly supporting partially applying functions / 'currying'
        --btw, we might not be fully error checking here.  to me it looks like the above only checks whether the numargs==numparams IF varargs is not present.  if varargs is present, we should ideally check that the args >= totalParams.
            --the resulting error checking might be done in the "drop (length params) args" function
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
        --first, bind the multiple parameter-argument pairs to the closure environment.  pass that env along to bind the possible variadic argument.  then pass that env along to evalBody!
    where 
        remainingArgs = drop (length params) args --get remaining args after parameters are matched
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body --recall, body is a list of LispVal.  we're mapping eval, with the closure env, to every LispVal in this list.  then the result is the last expression eval'd in that list.
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)] --bind the variadic parameter name to the list of remaining args
            Nothing -> return env --pass the env through unchanged

readOrThrow :: Parser a -> String -> ThrowsError a --for reading FROM FILES
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr --specialization of readOrThrow to read single expressions
readExprList = readOrThrow (endBy parseExpr spaces) --for use within loading files

primitiveBindings :: IO Env --create the IO Env values from the original list of primitives (which themselves are just tuples of (String, [LispVal] -> ThrowsError LispVal)
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
    where 
        makeFunc constructor (var, func) = (var, constructor func)
        --makeFunc takes the argument "constructor" to decide whether it takes IOFunc or PrimitiveFunc
        -- note: I think IOFunc and Func have different type (ThrowsError and IOThrowsError) but we wrote "apply" such that it knows how to handle this.
        --start with nullEnv (empty environment), and bind all the primitives we defined to it.

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
    ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [
    ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
    ]
--note here: read-contents and read-all take String filename, whereas read takes port! using the wrong thing here gives a Haskell runtime error

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal --openFile is the haskell function openFile :: FilePath -> IOMode -> IO Handle
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
--"intended to be partially-applied to the IOMode: ReadMode for open-input-file and WriteMode for open-output-file"

closePort :: [LispVal] -> IOThrowsError LispVal --hClose is the haskell function hClose :: Handle -> IO ()
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True) --when we match a port, close it then return true
closePort _ = return $ Bool False --if we didn't match a port, return false?

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc _ = return $ Bool False --without this, we get a runtime error if user accidentally uses String instead of Port
    --ideally we would figure out how to wrap this into the error handling typical of this implementation, but for now we'll just return #f.
--hGetLine is the haskell function hGetLine :: Handle -> IO String

writeProc :: [LispVal] -> IOThrowsError LispVal --converts a LispVal to a string and then writes it out on the specified port:
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal --haskell's readFile just does what it sounds like
--readFile :: FilePath -> IO String (but as usual, with haskell IO, we don't return a String, we return an IO String - the promise of computing a string when requested, roughly.)
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal] --read in a file of statements.  note: this is not Scheme's (load ).  this just reads a file.
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal --wraps the result of load into a List (LispVal constructor) and lift to IOThrowsError
readAll [String filename] = liftM List $ load filename

--generic binary boolean operation, we use this to more easily capture the various cases
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
    then throwError $ NumArgs 2 args
    else do --this do block is for the Error monad (stuff can throw a type mismatch)
        left <- unpacker $ args !! 0
        right <- unpacker $ args !! 1
        return $ Bool $ left `op` right --`infix version` of a function like <= etc

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

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

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s --silently convert non string to string representation
unpackStr (Bool s)   = return $ show s --silently convert non string to string representation
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)



--list stuff (wow, car and cdr are such great names that everyone understands)
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x --Dotted list a [LispVal] LispVal - a list of LispVals, followed by another LispVal (captured as _ here)
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [throwaway] x] = return x
cdr [DottedList (throwaway : xs) x] = return $ DottedList xs x -- get rid of head of list, keep dotted structure
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList
--(cdr '(4))     ;Value: () ........ do we handle this case correctly?

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1] --cons'ing x1 with an empty list lulz.  but we see it works: scheme: (cons 5 '())    ;Value: (5)
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast --(cdr '(4 5 6 . 7))    ;Value: (5 6 . 7)
cons [x1, x2] = return $ DottedList [x1] x2 --(cons 5 6)    ;Value: (5 . 6)
cons badArgList = throwError $ NumArgs 2 badArgList --cons more or less than 2 things is an error

--equivalence: scheme has (eqv? obj1 obj2), (eq? obj1 obj2) , and (equal? obj1 obj2)
--eq? is the finest or most discriminating, and equal? is the coarsest. eqv? is slightly less discriminating than eq?.
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2 --pretty obvious
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2 --pretty obvious
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2 --wait are these all just gonna be pretty obvious
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2 --ok maybe these are all rly obvious
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]] --ah , just as i thought , this one is more complex.  recurses down to the List case tho.
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2) --first, check of lengths equal.  then check of all values are ... all... eqvPair :)
    where eqvPair (x1, x2) = case eqv [x1, x2] of   Left err -> False --recurse to apply eqv to a list of 2 LispVals
                                                    Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

--equal? , being the coarsest equivalence relation, should ignore type tags (if possible) & thus be "weakly typed" meh
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
    do 
        unpacked1 <- unpacker arg1
        unpacked2 <- unpacker arg2
        return $ unpacked1 == unpacked2 --unpacked1==unpacked2 bool, this is lifted into ThrowsError
    `catchError` (const $ return False)
    --catchError :: MonadError e m => m a -> (e -> m a) -> m a
--takes an Either action and a function that turns an error into another Either action.
--here we use it as an infix between the result of the 'do' block there and a (const $ return False) where return is lifting to ThrowsError monad
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2] -- want True on a superset of eqv true.  so use eqv directly out of laziness.
    --but note that this laziness is bad.  bc now eqv will recurse into recursed lists, instead of 'equal'.  so this would be something to improve in the future.
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
--ok so the first line of that "do" block: see if the things evaluate as equal from any possible unpacking (that is the "or").  the list literal is the list of unpackers tried, and the function is the partially-applied (to the values in equations) unpackEquals
--third line looks confusing.  it pattern matches using (Bool x) to extract the result of the eqvEquals binding.  then lifts this result as a ThrowsError Bool (Bool is a LispVal data constructor)

main :: IO ()
main = do 
    args <- getArgs
    case length args of
        0 -> runRepl --if no arguments, enter REPL!
        otherwise -> runOne $ args 
