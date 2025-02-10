module Error where
import Tokenize
import Data.List
import Tokenize
import System.IO

data Error = ErrorCode Int
           | ErrorText String
           | ErrorLoc Int String
           | ErrorToken Token String
           deriving Show

errorAt :: String -> Int -> String -> IO (Int)
errorAt current_input loc text = do
  hPutStrLn stderr $ current_input
  hPutStrLn stderr $ replicate loc ' ' ++ "^ " ++ text
  return 1

errorTok :: String -> Token -> String -> IO (Int)
errorTok input tok text = errorAt input (tokenLoc tok) $ text ++ "\n"  ++ show tok



printError :: String -> Error -> IO (Int)
printError input (ErrorToken t text) = do
        let lines = getLines input 0 0
        let iLines = zip[0..] lines
        let line = ignoreUnknownLine (find (finder (tokenLoc t)) iLines)

        hPutStrLn stderr $ "stdin:" ++ show (fst line) ++ ": " ++ text ++ " " ++ (show (tokenKind t))
        errorTok input t text
    where
      finder :: Int -> ((Int, (Int, Int)) -> Bool)
      finder x (_, (_, e)) = x < e

      -- FIXME: unreachiable - think about it
      ignoreUnknownLine (Just x) = x
      ignoreUnknownLine Nothing = (0,(0, 0))
printError input (ErrorLoc loc text) = do
        errorAt input loc text
        return 1
printError input (ErrorText text) = do
        let lines = getLines input 0 0
        hPutStrLn stderr text
        return 1
