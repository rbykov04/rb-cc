module Main where


import System.Environment
import System.IO
import RBCC
import Codegen
import Tokenize
import Parse
import Parse2
import Error
import Text.Printf
import Data.List
import Data.List.Split

import Data.IntMap (elems)
--import qualified MyLib (someFunc)


printlnProgram :: [String] -> IO ()
printlnProgram [] = return ()
printlnProgram (s:ss) = do
  printf "%s\n" s
  printlnProgram ss


nameNode_ :: Node_ -> String
nameNode_ (BLOCK x)      = "BLOCK"
nameNode_ (STMT_EXPR x)  = "STMT_EXPR"
nameNode_ (EXPS_STMT x)  = "EXPR_STMT"
nameNode_ (IF _ _ _)  = "IF"
nameNode_ (FUNCALL name _)  = "FUNCALL " ++ name
nameNode_ (UNARY _ _)  = "UNARY"
nameNode_ (BIN_OP _ _ _)  = "UNARY"
nameNode_ (RETURN _)  = "RETURN"
nameNode_ (Assign _ _)  = "ASSIGN"
nameNode_ node =  show node

nameNode =   nameNode_ .  nodeNode

inode n =  (((tokenLoc . nodeToken) n), nameNode n)

--functor
visit :: (Node -> a) -> Node -> [a]
visit f node@(Node (BLOCK x) _ _)         = [f node] ++ (concatMap . visit) f x
visit f node@(Node (EXPS_STMT x) _ _)     = [f node] ++ visit f x
visit f node@(Node (STMT_EXPR x) _ _)     = [f node] ++ visit f x
visit f node@(Node (FUNCALL name x) a b)  = [f node] ++ (concatMap . visit) f x
visit f node = [f node]


onlyCall (FUNCALL _ _)  = True
onlyCall _  = False

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
  Just new -> Just ((Node (EXPS_STMT n)) a b)
  _ -> Just ((Node (EXPS_STMT n)) a b) -- it is imbosible
ignore x = Nothing



queryFuncCall :: [Node] -> [Node]
queryFuncCall [] = []
queryFuncCall (x:xs) = case ignore x of
  Nothing  -> queryFuncCall xs
  Just r  -> r : queryFuncCall xs

printObj :: Obj -> [String]
printObj obj =
    let
      t            = (typeKind . objType) obj
      body         = objBody obj
      printBody    = (concatMap . visit) nameNode
      printBody1   = (concatMap . visit) (\x -> show (nameNode x, 10))
      filteredBody = queryFuncCall body

    in case t of
      INT -> [objName obj ++ " " ++ show t]
      _ ->
        [objName obj ++ " " ++ show t]
        ++ ["body->"]
        ++ (printBody (objBody obj))
        ++ ["############", "filtered body->"]
        ++ printBody1 filteredBody

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

console :: String -> [Obj] -> [String]
console file ob =
    let
      iLines = zip[0..] $ getLines file 0 0
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


numerize = zip[0..]

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
collapse ((line, i, x) : xs) =
  let
    res = collapse (xs)
  in case res of
    [] -> [(line, [x])]
    ((l, acc): xs) -> if line == l
                      then (line, x : acc) : xs
                      else (line, [x]) : (l, acc) : xs


toksToMargin :: [Int] -> [(Int, [String])] -> [String]
toksToMargin [] _ = []
toksToMargin (i: is) toks = [show (getToks i toks)] ++ toksToMargin is toks
  where
    getToks :: Int -> [(Int, [String])] -> [String]
    getToks i [] = []
    getToks i ((line, toks) :  xs) =
      if i == line
      then toks ++ getToks i xs
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
    max = maxrec sizes
    adds = map (\size -> replicate (max - size) ' ') sizes
  in mergeColumnsArr [arr, adds]

toPurple = "\ESC[1;35m"
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
      lines = getLines file 0 0
      iLines = zip[0..] lines
      obj2_ = printSt2 ob
      obj3_ = map (addLine iLines) obj2_
      text = splitOn "\n" file

      numbers       = take (length text ) [0..]
      delim         = map (const " ") numbers
      marginLeft    = map show numbers
      nums          = (toksToMargin numbers (collapse obj3_))
      toks          = marginLeft
      coloredText   = splitOn "\n"  $ toHighlight file 95 100
      x             = map show iLines
      cols          =
        [
          normalizeCol toks,
          delim, normalizeCol nums,
         -- delim, normalizeCol x,
          delim, coloredText
        ]
    in mergeColumnsArr cols

main :: IO (Int)
main = do
  hPutStrLn stdout "============================="
  file <- getContents
  hPutStrLn stdout file
  hPutStrLn stdout "============================="
  let res = tokenize_ file
  case res of
    Left (loc, text) -> do
      errorAt file loc text
    Right toks -> do

      let t = concatMap (\x -> [(show x)]) toks
      printlnProgram t
      hPutStrLn stdout "============================="

      let parse_res = (parse . convert_keywords) toks
      case parse_res of
        Left err -> printError file err
        Right (globals, _, storage) ->
          case codegen globals storage of
          Right prog -> do
            hPutStrLn stdout "============================="
            printlnProgram (map show globals)
            hPutStrLn stdout "============================="
            -- printlnProgram (map show storage)
            hPutStrLn stdout "============================="
            let st = elems storage
            hPutStrLn stdout "============================="
            printlnProgram $ console file globals
            hPutStrLn stdout "============================="
            hPutStrLn stdout $ show globals
            hPutStrLn stdout "============================="
            printlnProgram $ codeView file st
            let prog_ = (parse2 . convert_keywords) toks

            case prog_ of
              Left err -> printError file err
              Right prog -> do
                hPutStrLn stdout $ show prog
                return 0


            return 0

          Left e -> do
            printError file e
