module Parse2 where
import Error
import Tokenize
import Stage1


parse2 :: [Token] -> Either Error Program
parse2 toks = do program  (removeEOF toks)

