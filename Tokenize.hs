module Tokenize where
import System.IO
import Data.Char
import Data.List
import RBCC
import Control.Monad.Trans.Except
import Control.Monad.State

error_at :: String -> Int -> String -> IO (Int)
error_at current_input loc text = do
  hPutStrLn stderr $ current_input
  hPutStrLn stderr $ replicate loc ' ' ++ "^ " ++ text
  return 1

error_tok :: String -> Token -> String -> IO (Int)
error_tok input tok text = error_at input (tokenLoc tok) $ text ++ "\n"  ++ show tok

printError :: String -> Error -> IO (Int)
printError input (ErrorToken t text) = do
  error_tok input t text
printError input (ErrorText text) = do
  hPutStrLn stderr $ text
  return 1

type TokenState = (String, Int, [Token])

seeCharNum = do
  (s,num,_) <- get
  if length s == 0 then
    throwE (num, "unexpected end")
  else
    return $ (head s, num)

seeChar = do
  (c, _) <- seeCharNum
  return c

isIdent1 :: Char -> Bool
isIdent1 c = isAlpha c || c == '_'

isIdent2 :: Char -> Bool
isIdent2 c = isIdent1 c || isDigit c

popCharNum :: ExceptT  (Int, String) (State TokenState) (Char, Int)
popCharNum = do
  (s,num,res) <- get
  if length s == 0 then
    throwE (num, "unexpected end")
  else do
    put (tail s, num+1, res)
    return $ (head s, num)

popChar = fmap fst popCharNum

addToken :: Token -> ExceptT  (Int, String) (State TokenState) ()
addToken tok = get >>= (put . f tok) where
  f tok (s, num, res) = (s, num, res ++ [tok])

readWordM_ :: Int -> String -> ExceptT  (Int, String) (State TokenState) ()
readWordM_ begin str = do
  (c, num) <- seeCharNum
  if isIdent2 c then do
    ident <- popChar
    readWordM_ begin (str ++ [ident])
  else do
    addToken $ Token (Ident str) (num - begin) begin

toArray = (: [])
readWordM (c, num) = ((readWordM_ num) . toArray) c

readPunct (c, num) = addToken $ Token (Punct [c]) 1 num

readCompoundPunct :: (Char, Int) -> ExceptT  (Int, String) (State TokenState) ()
readCompoundPunct (p, begin) = do
  (ps, _, _) <- get
  if length ps == 0 then
    addToken $ Token (Punct [p]) 1 begin
  else if (p == '>' && head ps == '=')
       || (p == '<' && head ps == '=')
       || (p == '!' && head ps == '=')
       || (p == '=' && head ps == '=')
  then do
    _ <- popChar
    addToken $ Token (Punct (p:[head ps])) 2 begin
  else
    addToken $ Token (Punct [p]) 1 begin

readNumM_ :: Int -> Int -> Int -> ExceptT  (Int, String) (State TokenState) ()
readNumM_ begin count number = do
  (c, num) <- seeCharNum
  if isDigit c && count > 0 then do
    x <- popChar
    readNumM_ begin (count -1) (number* 10 + digitToInt x)
  else do
    addToken $ Token (Num number) (num - begin) begin

readNumM (c, num)= readNumM_ num 10 (digitToInt c)

maybeEnd = do
  (s, num, _) <- get
  if length s == 0 then
    addToken $ Token EOF 0 num
  else
    tokenizeM

tokenizeM ::ExceptT  (Int, String) (State TokenState) ()
tokenizeM = do
  (c, num) <- seeCharNum
  if      isSeparator c   then popCharNum >> maybeEnd
  else if isIdent1 c      then popCharNum >>= readWordM         >> maybeEnd
  else if c =='>'         then popCharNum >>= readCompoundPunct >> maybeEnd
  else if c =='<'         then popCharNum >>= readCompoundPunct >> maybeEnd
  else if c =='!'         then popCharNum >>= readCompoundPunct >> maybeEnd
  else if c =='='         then popCharNum >>= readCompoundPunct >> maybeEnd
  else if isPunctuation c then popCharNum >>= readPunct         >> maybeEnd
  else if isSymbol c      then popCharNum >>= readPunct         >> maybeEnd
  else if isDigit c       then popCharNum >>= readNumM          >> maybeEnd
  else
    throwE (num, "invalid token" ++ [c])

tokenize_ str =
  let (r, (_, _, toks)) = runState (runExceptT tokenizeM) (str, 0, [])
      result = case r of
        Left e -> Left e
        Right _ -> Right toks
  in result


get_number :: String -> Token -> IO (Maybe Int)
get_number input (Token (Num v) _ _) = return (Just v)
get_number input tok = do
  error_tok input tok "expected a number"
  return Nothing

keywords = ["return", "if", "else", "for", "while", "char", "int", "sizeof"]

convert_keywords :: [Token] -> [Token]
convert_keywords = map toKeyword
  where
    toKeyword tok@(Token (Ident name) a b) =
      case find (== name) keywords of
        Nothing ->  tok
        Just _ -> (Token (Keyword name) a b)
    toKeyword tok = tok
