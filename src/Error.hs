module Error where
import Data.List
import Tokenize

data Error = ErrorCode Int
           | ErrorText String
           | ErrorLoc Int String
           | ErrorToken Token String
           deriving Show


errorAt :: String -> Int -> String -> [String]
errorAt current_input loc text = [current_input] ++ [replicate loc ' ' ++ "^ " ++ text]

errorTok :: String -> Token -> String -> [String]
errorTok input tok text = errorAt input (tokenLoc tok) $ text ++ "\n"  ++ show tok


printError :: String -> Error -> [String]
printError input (ErrorToken t text) =
        let
          lines = getLines input 0 0
          iLines = zip[0..] lines
          line = ignoreUnknownLine (find (finder (tokenLoc t)) iLines)
          a = "stdin:" ++ show (fst line) ++ ": " ++ text ++ " " ++ (show (tokenKind t))
          b = errorTok input t text
        in [a] ++ b
          where
            finder :: Int -> ((Int, (Int, Int)) -> Bool)
            finder x (_, (_, e)) = x < e

            -- FIXME: unreachiable - think about it
            ignoreUnknownLine (Just x) = x
            ignoreUnknownLine Nothing = (0,(0, 0))
printError input (ErrorLoc loc text) = errorAt input loc text
printError input (ErrorText text) = [text]
