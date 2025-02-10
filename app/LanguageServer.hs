module Main where
import System.IO ()
import RBCC (
            Node_ (..)
            , Error (..)
            , TypeKind (..)
            , Type (..)
            , Obj (..)
            , Node (..)
            )
import Codegen (codegen)
import Tokenize ( tokenize_
                , getLines
                , convert_keywords
                , Token (..)
                )
import Parse (parse)
import Parse2
import Error (printError)
import Text.Printf (printf)
import Data.List.Split (splitOn)

import Data.IntMap (elems)
--import qualified MyLib (someFunc)


printlnProgram :: [String] -> IO ()
printlnProgram [] = return ()
printlnProgram (s:ss) = do
  printf "%s\n" s
  printlnProgram ss


nameNode_ :: Node_ -> String
nameNode_ (BLOCK _)      = "BLOCK"
nameNode_ (STMT_EXPR _)  = "STMT_EXPR"
nameNode_ (EXPS_STMT _)  = "EXPR_STMT"
nameNode_ (IF _ _ _)  = "IF"
nameNode_ (FUNCALL name _)  = "FUNCALL " ++ name
nameNode_ (UNARY _ _)  = "UNARY"
nameNode_ (BIN_OP _ _ _)  = "UNARY"
nameNode_ (RETURN _)  = "RETURN"
nameNode_ (Assign _ _)  = "ASSIGN"
nameNode_ node =  show node

nameNode :: Node -> String
nameNode =   nameNode_ .  nodeNode

inode :: Node -> (Int, String)
inode n =  (((tokenLoc . nodeToken) n), nameNode n)

--functor
visit :: (Node -> a) -> Node -> [a]
visit f node@(Node (BLOCK x) _ _)         = [f node] ++ (concatMap . visit) f x
visit f node@(Node (EXPS_STMT x) _ _)     = [f node] ++ visit f x
visit f node@(Node (STMT_EXPR x) _ _)     = [f node] ++ visit f x
visit f node@(Node (FUNCALL _ x) _ _)  = [f node] ++ (concatMap . visit) f x
visit f node = [f node]

onlyCall :: Node_ -> Bool
onlyCall (FUNCALL _ _)  = True
onlyCall _  = False

onlyBlock :: Node_ -> Bool
onlyBlock (BLOCK _)  = True
onlyBlock _  = False

cmbOR :: [(a -> Bool)] -> a -> Bool
cmbOR []       _   = False
cmbOR (f : fs) arg = if f arg then True else cmbOR fs arg


--filter ((cmbOR [onlyCall, onlyBlock])  . nodeNode)
ignore :: Node -> Maybe Node
ignore (Node (BLOCK n) a b)  = Just (Node (BLOCK (queryFuncCall n)) a b)
ignore (Node (FUNCALL name args) a b) = Just ((Node (FUNCALL name args)) a b)
ignore (Node (EXPS_STMT n) a b) = case ignore n of
  Just _ -> Just ((Node (EXPS_STMT n)) a b)
  _ -> Just ((Node (EXPS_STMT n)) a b) -- it is imbosible
ignore _ = Nothing



queryFuncCall :: [Node] -> [Node]
queryFuncCall [] = []
queryFuncCall (x:xs) = case ignore x of
  Nothing  -> queryFuncCall xs
  Just r  -> r : queryFuncCall xs

printObj :: Obj -> [String]
printObj obj =
    let
      t            = (typeKind . objType) obj
      --body         = objBody obj
      printBody    = (concatMap . visit) nameNode
      --printBody1   = (concatMap . visit) (\x -> show (nameNode x, 10))
      --filteredBody = queryFuncCall body

    in case t of
      INT -> [objName obj ++ " " ++ show t]
      _ ->
        [objName obj ++ " " ++ show t]
        ++ ["body->"]
        ++ (printBody (objBody obj))
       -- ++ ["############", "filtered body->"] ++ printBody1 filteredBody

printObj1 :: Obj -> [String]
printObj1 obj =
      let
        name = objName obj
        output = name
          ++ " "
           ++ (show . typeKind . objType ) obj
      in [output]


objIsFunc :: Obj -> Bool
objIsFunc obj = case (typeKind . objType) obj of
    FUNC _ _ _ _ -> True
    _            -> False

numerize :: [a] -> [(Int, a)]
numerize = zip[0..]

console :: String -> [Obj] -> [String]
console _ ob =
    let
      obj = (concatMap printObj) ob
      globalObj = filter (not . objIsLocal) ob
      globalVars = filter (not . objIsFunc) globalObj
      obj1 = (map show) globalVars
      obj2 = (concatMap printObj1) globalVars
    in concat
    [ obj
    , ["Objects glo"]
    , obj1
    , ["Objects"]
    , obj2
    ]


mergeColumns :: [String] -> [String] -> [String]
mergeColumns [] a = a
mergeColumns a [] = a
mergeColumns ( l :ls) (r :rs) = (l ++ r) : mergeColumns ls rs

addLine :: [(Int, (Int, Int))] -> (Int, a) -> (Int, Int, a)
addLine [] (loc, x)                   = (-1, loc, x)
addLine ((line, (b, e)): xs) (loc, x) =
  if loc >= b && loc < e then (line, loc, x) else addLine xs (loc, x)

collapse :: [(Int, Int, x)] -> [(Int, [x])]
collapse [] = []
collapse ((line, _, x) : xs) =
  let
    res = collapse (xs)
  in case res of
    [] -> [(line, [x])]
    ((l, acc): xs_) -> if line == l
                      then (line, x : acc) : xs_
                      else (line, [x]) : (l, acc) : xs_


toksToMargin :: [Int] -> [(Int, [String])] -> [String]
toksToMargin [] _ = []
toksToMargin (i: is) toks = [show (getToks i toks)] ++ toksToMargin is toks
  where
    getToks :: Int -> [(Int, [String])] -> [String]
    getToks _ [] = []
    getToks i_ ((line, toks_) :  xs) =
      if i_ == line
      then toks_ ++ getToks i_ xs
      else getToks i xs

maxrec :: (Ord a) => [a] -> a
maxrec [] = error "maximum of empty list"
maxrec [x] = x
maxrec (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maxrec xs

normalizeCol :: [String] -> [String]
normalizeCol arr =
  let
    sizes = map length arr
    max_ = maxrec sizes
    adds = map (\size -> replicate (max_ - size) ' ') sizes
  in mergeColumnsArr [arr, adds]

toPurple :: String
toPurple = "\ESC[1;35m"

toDefault :: String
toDefault = "\ESC[0m"

-- how cares about error Hanlding?
toHighlight :: String -> Int -> Int -> String
toHighlight text b e =
  let
    (t, right) = splitAt e text
    (left, str) = splitAt b t
  in left ++ toPurple ++ str ++ toDefault ++ right



mergeColumnsArr :: [[String]] -> [String]
mergeColumnsArr []     = []
mergeColumnsArr (s:ss) = mergeColumns s (mergeColumnsArr ss)

codeView :: String -> [Obj] -> [String]
codeView file ob =
    let
      printSt2 = concatMap (((concatMap . visit)  inode) . objBody)
      lines_ = getLines file 0 0
      iLines = zip[0..] lines_
      obj2_ = printSt2 ob
      obj3_ = map (addLine iLines) obj2_
      text = splitOn "\n" file

      numbers       = take (length text ) [0..]
      delim         = map (const " ") numbers
      marginLeft    = map show numbers
      nums          = (toksToMargin numbers (collapse obj3_))
      toks          = marginLeft
      coloredText   = splitOn "\n"  $ toHighlight file 95 100
       -- x             = map show iLines
      cols          =
        [
          normalizeCol toks,
          delim, normalizeCol nums,
         -- delim, normalizeCol x,
          delim, coloredText
        ]
    in mergeColumnsArr cols

rowLine :: String
rowLine = "============================="

getGlobalVars :: Program -> [VarDecl]
getGlobalVars (Program []) = []
getGlobalVars (Program decl) = concatMap visDecl decl
  where
    visDecl (VariableDecl _ _ v) = v
    visDecl _  = []

getFuncs :: Program -> [Decl]
getFuncs (Program []) = []
getFuncs (Program decl) = concatMap visDecl decl
  where
    visDecl v@(Function _ _ _ _) = [v]
    visDecl _  = []




getVarLoc :: VarDecl -> (Token, String)
getVarLoc (Variable (Context tok) name _) = (tok, name)

getFuncLoc :: Decl -> (Token, String)
getFuncLoc (Function (Context tok) _ name _) = (tok, name)



--anotherParser :: String -> [Token] -> Either Error [String]
anotherParser _ toks = do
  prog_ <- (parse2 . convert_keywords) toks
  let vars = getGlobalVars prog_
  let vars2 = map getVarLoc vars
  return vars2

  --anotherParser :: String -> [Token] -> Either Error [String]
anotherParser2 _ toks = do
  prog_ <- (parse2 . convert_keywords) toks
  let funcs = getFuncs prog_
  return $ map getFuncLoc funcs



showTokens :: String -> [Token] -> [String]
showTokens file toks =
  let
    t = concatMap (\x -> [(show x)]) toks
    f = splitOn "\n" file
  in
    rowLine : (f ++ t)

errAdapter :: Either (Int, String) a -> Either Error a
errAdapter (Left (loc, text)) = Left $ ErrorLoc loc text
errAdapter (Right a) = Right a

highlight :: String -> [(Int, Int)] -> String
highlight text [] = text
highlight text ((b, e) : ss) =
    let
      coloredText = highlight text ss
    in
      toHighlight coloredText b e

codeView2 :: String -> [(Token, String)] -> [String]
codeView2 file parts =
    let
      arr = map (\(t, _) -> (tokenLoc t , tokenLoc t + tokenLen t)) parts
      --printSt2 = concatMap (((concatMap . visit)  inode) . objBody)
      --lines_ = getLines file 0 0
      --iLines = zip[0..] lines_
      --obj2_ = printSt2 ob
      --obj3_ = map (addLine iLines) obj2_
      text = splitOn "\n" file

      numbers       = take (length text ) [0..]
      delim         = map (const " ") numbers
      nums    = map show numbers
      --nums          = (toksToMargin numbers (collapse obj3_))
     -- toks          = marginLeft
      coloredText   = splitOn "\n"  $ highlight file arr
       -- x             = map show iLines
      cols          =
        [
         normalizeCol nums,
         -- delim, normalizeCol nums,
         -- delim, normalizeCol x,
          delim, coloredText
        ]
    in mergeColumnsArr cols



parserStage :: String -> Either Error [String]
parserStage file = do
  toks                  <- (errAdapter . tokenize_) file
  (globals, _, storage) <- (parse . convert_keywords) toks
  vars                <- anotherParser file toks
  funcs               <- anotherParser2 file toks
  prog_ <- (parse2 . convert_keywords) toks

  let res = map show vars
  _  <- codegen globals storage
  -- let x = (map show globals)
  let st = elems storage
  return
    $  [rowLine]
    -- ++ x
   -- ++ [rowLine] ++ showTokens file toks ++ [rowLine]
    ++ [rowLine]
    ++ (codeView file st)
    ++ [rowLine]
    ++ res
    ++ [rowLine]
    ++ (codeView2 file vars)
    ++ [rowLine]
    ++ (codeView2 file funcs)
    ++ [rowLine] ++ [show prog_]
    ++ [rowLine]

main :: IO (Int)
main = do
  file <- getContents
  case parserStage file of
    Left e -> do
      printError file e
    Right text -> do
      printlnProgram text
      return 0
