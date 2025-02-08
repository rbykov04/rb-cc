module Parse2 where
import RBCC
import Data.List



newtype Context = Context Token
  deriving (Show, Eq)

mkCtx :: Token -> Context
mkCtx = Context

data Declspec = DeclInt Context
              | DeclChar Context
              deriving (Show, Eq)

data VarDecl = Pointer Context String Declspec
             | Varible Context String Declspec
             deriving (Show, Eq)

data Decl = Function
          | VariableDecl Context Declspec [VarDecl]
          deriving (Eq, Show)

--TODO think about dubliaction type of var in Decl and VarDecl
mkVarDec ty varArr tok = VariableDecl (mkCtx tok) ty varArr

newtype Program = Program [Decl] deriving (Eq, Show)


headToken :: [Token] -> Either Error (Token, [Token])
headToken [] = Left $ ErrorText "Internal logic: headToken unreachable"
headToken (s : ss)  = Right (s , ss)



mkInt  = DeclInt . mkCtx
mkChar = DeclChar . mkCtx
-- declspec = "int" | "char"
declSpec   :: [Token] -> Either Error (Declspec, [Token])
declSpec toks = do
  (tok, toks) <- headToken toks

  case tokenKind tok of
    Keyword "int"  -> Right $  (mkInt tok, toks)
    Keyword "char" -> Right $  (mkChar tok, toks)
    _              -> Left  $ ErrorToken tok "Unkown type in declaration"


mkTokError tok err = Left $ ErrorToken tok err
mkTextError err = Left $ ErrorText err

skipTKind :: TokenKind -> [Token] -> Either Error [Token]
skipTKind kind [] = mkTextError $ "Unexpected skip of " ++ show kind
skipTKind kind (t : ts) = if tokenKind t == kind
                          then Right ts
                          else mkTokError t ("expected " ++ show kind)

-- declarator = "*"* ident type-suffix
mkVar decl name tok = Varible (mkCtx tok) name decl

declarator2 :: Declspec -> [Token] -> Either Error (VarDecl, [Token])
declarator2 ty toks = do
  -- ty <- ptr_wrap basetype FIXME
  (tok, toks) <- headToken toks
  case tok of
    Token (Ident name) _ _ -> do
      -- decl_type <- type_suffix ty FIXME
      return $ (mkVar ty name tok, toks)
    _ -> mkTokError tok "expected an identifier"
  where
    {-
    ptr_wrap base = do
      isPtr <- consume (Punct "*")
      if isPtr
      then ptr_wrap $ pointer_to base
      else return base
-}

tbd s = mkTextError $ "to be continued" ++ show s


lookAhead :: [Token] -> Either Error Token
lookAhead toks = do
    (tok, _) <- headToken toks
    return tok


globalVariable :: [Token] -> Either Error (Decl, [Token])
globalVariable toks = do
  fstTok <- lookAhead toks
  (ty, toks) <- declSpec toks
  (vars, toks) <- iter ty toks []
  toks <- skipTKind (Punct ";") toks
  return (mkVarDec ty vars fstTok, toks)
  where
    iter ty toks acc = do
      (v, toks) <- declarator2 ty toks
      tok <- lookAhead toks
      case tokenKind tok of
        Punct "," -> do
          toks <- skipTKind (Punct ",") toks
          iter ty toks (v : acc)
        _         -> pure (v: acc, toks)


program2 :: [Token] -> Either Error (Program, [Token])
program2 [] = mkTextError "Unexpected end of program"
program2 toks@(s : ss) = case tokenKind s of
  EOF -> return (Program [], [])
  _ -> do
    (vars, toks) <- globalVariable toks
    (Program decl, _) <- program2 toks
    return (Program (vars: decl) , toks)


parse2 :: [Token] -> Either Error Program
parse2 toks = do
  (prog, _) <- program2 toks
  return prog

