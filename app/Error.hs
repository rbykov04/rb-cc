module Error where
import RBCC
import Data.List
import Tokenize (getLines)
import System.IO

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
printError input (ErrorText text) = do
        let lines = getLines input 0 0
        hPutStrLn stderr text
        return 1
