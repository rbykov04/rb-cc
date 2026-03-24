{-
This MetaIR is a 'meta' IR to pretty print others IR of rb-cc

It might be useless (myabe not :D)

There is support:
1. print
2. assertEq
3. class to serialize
-}

module MetaIR where
import Context
import Printable
import Error

data MetaIR = LeafIR      Context String                        [String]
            | NodeUnIR    Context String MetaIR                 [String]
            | NodeBinIR   Context String MetaIR MetaIR          [String]
            | NodeListIR  Context String [MetaIR]               [String]
            | NodeList1IR Context String MetaIR        [MetaIR] [String]
  deriving (Show, Eq)

getContext :: MetaIR -> Context
getContext  (LeafIR      ctx _ _)     = ctx
getContext  (NodeUnIR    ctx _ _ _)   = ctx
getContext  (NodeBinIR   ctx _ _ _ _) = ctx
getContext  (NodeListIR  ctx _ _ _)   = ctx
getContext  (NodeList1IR ctx _ _ _ _) = ctx

getFst :: MetaIR -> Maybe MetaIR
getFst  (LeafIR {})               = Nothing
getFst  (NodeUnIR _ _ lhs _)      = Just lhs
getFst  (NodeBinIR _ _ lhs _ _)   = Just lhs
getFst  (NodeListIR  {})          = Nothing
getFst  (NodeList1IR _ _ lhs _ _) = Just lhs

getSnd :: MetaIR -> Maybe MetaIR
getSnd  (LeafIR {})               = Nothing
getSnd  (NodeUnIR {})             = Nothing
getSnd  (NodeBinIR _ _ _ rhs _)   = Just rhs
getSnd  (NodeListIR  {})          = Nothing
getSnd  (NodeList1IR {})          = Nothing

getName :: MetaIR -> String
getName  (LeafIR      _ name _)     = name
getName  (NodeUnIR    _ name _ _)   = name
getName  (NodeBinIR   _ name _ _ _) = name
getName  (NodeListIR  _ name _ _)   = name
getName  (NodeList1IR _ name _ _ _) = name

getLoad :: MetaIR -> [String]
getLoad  (LeafIR      _ _ loads)     = loads
getLoad  (NodeUnIR    _ _ _ loads)   = loads
getLoad  (NodeBinIR   _ _ _ _ loads) = loads
getLoad  (NodeListIR  _ _ _ loads)   = loads
getLoad  (NodeList1IR _ _ _ _ loads) = loads

getType :: MetaIR -> String
getType  (LeafIR      _ _ _)     = "LeafIR"
getType  (NodeUnIR    _ _ _ _)   = "NodeUnIR"
getType  (NodeBinIR   _ _ _ _ _) = "NodeBinIR"
getType  (NodeListIR  _ _ _ _)   = "NodeListIR"
getType  (NodeList1IR _ _ _ _ _) = "NodeList1Ir"

getChildren :: MetaIR -> Maybe [MetaIR]
getChildren  (LeafIR {})               = Nothing
getChildren  (NodeUnIR {})             = Nothing
getChildren  (NodeBinIR {})            = Nothing
getChildren  (NodeListIR  _ _ chl _)   = Just chl
getChildren  (NodeList1IR _ _ _ chl _) = Just chl


printLoad' :: Int -> [String] -> [String]
printLoad' _ [] = []
printLoad' tab load =
    ["Loads {"]
    ++ addTab (tab+1) load
    ++ ["}"]

printLoad :: Int -> [String] -> [String]
printLoad _ [] = []
printLoad tab load = ["load = " ++ concat  load]



instance Printable MetaIR where
  pprint :: MetaIR -> [String]
  pprint _ = []

  prettyCodePrint :: Int -> MetaIR -> [String]
  prettyCodePrint tab a =
    let title = [getName a ++ " :: " ++ getType a]
        children = case getChildren a of
                      Nothing -> []
                      Just chl -> concatMap (prettyCodePrint (tab+1)) chl

        lhs = case getFst a of
                  Nothing -> []
                  Just v -> ["lhs = {"]
                              ++ addTab (tab +1) (prettyCodePrint (tab+1) v)
                              ++ ["}"]
        rhs = case getSnd a of
                  Nothing -> []
                  Just v -> ["rhs = {"]
                              ++ addTab (tab +1) (prettyCodePrint (tab+1) v)
                              ++ ["}"]



        body  = printLoad tab (getLoad a) ++ lhs ++ rhs ++ children

    in title ++ addTab (tab+1) body

assertArrEq :: [MetaIR] -> [MetaIR] -> Either (Context, Context) ()
assertArrEq [] [] = Right ()
assertArrEq a [] = Left (getContext (head a), mkNullCtxText "end of Arr")
assertArrEq [] b = Left (mkNullCtxText "End of arr", getContext (head b))
assertArrEq (a:as) (b: bs) = do
  assertEq a b
  assertArrEq as bs



assertEq :: MetaIR -> MetaIR -> Either (Context, Context) ()
assertEq (LeafIR ctxA nameA a) (LeafIR ctxB nameB b) =
  if nameA == nameB && a == b
  then Right ()
  else Left (ctxA, ctxB)
assertEq (NodeUnIR ctxA nameA vA a) (NodeUnIR ctxB nameB vB b) =
  if nameA == nameB && a == b
  then assertEq vA vB
  else Left (ctxA, ctxB)
assertEq (NodeBinIR ctxA nameA lA rA a) (NodeBinIR ctxB nameB lB rB b) =
  if nameA == nameB && a == b
  then do
    assertEq lA lB
    assertEq rA rB
  else Left (ctxA, ctxB)
assertEq (NodeListIR ctxA nameA vA a) (NodeListIR ctxB nameB vB b) =
  if nameA == nameB && a == b
  then do
    assertArrEq vA vB
  else Left (ctxA, ctxB)
assertEq (NodeList1IR ctxA nameA lA vA a) (NodeList1IR ctxB nameB lB vB b) =
  if nameA == nameB && a == b
  then do
    assertEq lA lB
    assertArrEq vA vB
  else Left (ctxA, ctxB)
assertEq a b = Left (getContext a, getContext b)


assertEquality :: MetaIR -> MetaIR -> Either Error ()
assertEquality a b =
      case assertEq a b of
        Left (c1, c2) -> do
                mkTextError "They are not Equal"
        Right ()  -> Right ()



class Serializable a where
  des :: a -> MetaIR
  ser :: MetaIR -> Either Error a
